#!/bin/sh

AUDIO_JOURNAL="${AUDIO_JOURNAL-$HOME/audio-journal}"

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

tag() {
    YEAR=$(date --date="$DATE" +%Y)
    read -p "Title: " TITLE
    OUT=$(with_title "$TITLE")
    cp "$1" "$OUT"
    id3v2 -D "$OUT"
    id3v2 \
        --artist=rootmos \
        --song="$TITLE" \
        --TCOM="Gustav Behm" \
        --TCOP="$YEAR Gustav Behm" \
        --WOAR="https://rootmos.github.io" \
        --WOAF="https://soundcloud.com/rootmos" \
        --TRDA="$DATE" \
        --year="$YEAR" \
        --genre=52 \
        "$OUT"
}

postprocess() {
    sox "$1" "$2"
    tag "$2"
    save_to_secondary "$2"
}

playback() {
    mpv "$1"
}

with_title() {
    if [ -z "$1" ]; then
        echo "$AUDIO_JOURNAL/$DATE.mp3"
    else
        echo "$AUDIO_JOURNAL/${DATE}_$(echo "$1" | tr ' ' '-').mp3"
    fi
}
