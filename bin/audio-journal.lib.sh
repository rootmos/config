#!/bin/sh

AUDIO_JOURNAL="${AUDIO_JOURNAL-$HOME/audio-journal}"

SUFFIX=.mp3

choose_takes_dir() {
    IFS=':' read -ra TAKES <<< "$AUDIO_JOURNAL_TAKES"
    for target in "${TAKES[@]}"; do
        if [ -d "$target" ]; then
            echo "$target"
            return
        fi
    done
    echo $AUDIO_JOURNAL/takes
}

TAKES=$(choose_takes_dir)
mkdir -p "$AUDIO_JOURNAL" "$TAKES"

take() {
    exec rec "$1"
}

preprocess() {
    sox "$1" "$2" norm -1
}

save_to_secondary() {
    IFS=':' read -ra SECONDARY <<< "$AUDIO_JOURNAL_SECONDARY"
    for target in "${SECONDARY[@]}"; do
        if [ -d "$target" ]; then
            rsync -ha --progress "$1" "$target"
        fi
    done
}

upload() {
    s3cmd put "$1" s3://rootmos-sounds
}

tag() {
    YEAR=$(date --date="$DATE" +%Y)
    LENGTH=$(soxi -D "$1")
    URL="https://rootmos-sounds.ams3.cdn.digitaloceanspaces.com/$1"
    id3v2  1>&2 -D "$1"
    id3v2 1>&2 \
        --artist=rootmos \
        --song="$TITLE" \
        --TCOM="Gustav Behm" \
        --TCOP="$YEAR Gustav Behm" \
        --WOAR="https://rootmos.io" \
        --WOAF="$URL" \
        --TRDA="$DATE" \
        --TLEN="$LENGTH" \
        --year="$YEAR" \
        --genre=52 \
        "$1"

    METADATA=$(echo "$1" | sed "s/$SUFFIX$/.json/")
    cat > "$METADATA" <<EOF
{
    "title": "$TITLE",
    "sha1": "$(sha1sum "$1" | cut -d' ' -f1)",
    "url": "$URL",
    "filename": "$1",
    "artist": "rootmos",
    "composer": "Gustav Behm",
    "date": "$DATE",
    "year": $YEAR,
    "length": $LENGTH
}
EOF
    echo "$METADATA"
}

postprocess() {
    sox "$1" "$2"
    read -p "Title: " TITLE
    OUT=$(with_title "$TITLE")
    cp "$2" "$OUT"
    METADATA=$(tag "$OUT")
    cat $METADATA
    save_to_secondary "$OUT"
    save_to_secondary "$METADATA"
    upload "$OUT"
    upload "$METADATA"
}

playback() {
    mpv "$1"
}

with_title() {
    if [ -z "$1" ]; then
        echo "$AUDIO_JOURNAL/$DATE$SUFFIX"
    else
        echo "$AUDIO_JOURNAL/${DATE}_$(echo "$1" | tr ' ' '-')$SUFFIX"
    fi
}
