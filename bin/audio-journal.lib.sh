PRE_FILTERS="norm"
POST_FILTERS="norm"

AUDIO_JOURNAL=${AUDIO_JOURNAL-$HOME/audio-journal}
SUFFIX=${AUDIO_JOURNAL_SUFFIX-.mp3}

choose_takes_dir() {
    IFS=':' read -ra TAKES <<< "${AUDIO_JOURNAL_TAKES-}"
    for target in "${TAKES[@]}"; do
        if [ -d "$target" ]; then
            echo "$target"
            return
        fi
    done
    echo $AUDIO_JOURNAL/takes
}

TAKES=${TAKES-$(choose_takes_dir)}
mkdir -p "$AUDIO_JOURNAL" "$TAKES"

take() {
    exec rec "$1"
}

preprocess() {
    sox "$1" "$2" $PRE_FILTERS
}

save_to_secondary() {
    if [[ -v AUDIO_JOURNAL_SECONDARY ]]; then
        IFS=':' read -ra SECONDARY <<< "$AUDIO_JOURNAL_SECONDARY"
        for target in "${SECONDARY[@]}"; do
            if [ -d "$target" ]; then
                rsync -ha --progress "$1" "$target"
            fi
        done
    fi
}

BUCKET=${AUDIO_JOURNAL_BUCKET-rootmos-sounds}
case "$MODE" in
    release) PREFIX= ;;
    *) PREFIX=$MODE/ ;;
esac

upload() {
    s3cmd put --acl-public "$1" "s3://$BUCKET/$PREFIX"
}

url() {
    echo "https://$BUCKET.ams3.cdn.digitaloceanspaces.com/$PREFIX$1"
}

tag() {
    DATE=$2
    TITLE=$3
    YEAR=$(date --date="$DATE" +%Y)
    LENGTH=$(soxi -D "$1")
    FILENAME=$(basename "$1")
    URL=$(url "$FILENAME")
    id3v2 1>&2 -D "$1"
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

    METADATA=$(sed "s/$SUFFIX$/.json/" <<< "$1")
    cat > "$METADATA" <<EOF
{
    "title": "$TITLE",
    "sha1": "$(sha1sum "$1" | cut -d' ' -f1)",
    "url": "$URL",
    "filename": "$FILENAME",
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
    DATE=$2
    TITLE=$3
    unset OUT
    if [ "$MODE" = "release" ]; then
        if [ -z "$TITLE" ]; then
            read -p "Title: " TITLE
        fi

        if [ -n "$TITLE" ]; then
            OUT=$AUDIO_JOURNAL/${DATE}_$(tr ' ' '-' <<< "$TITLE")$SUFFIX
        fi
    fi

    if [[ ! -v OUT ]]; then
        mkdir -p "$AUDIO_JOURNAL/$MODE"
        OUT=$AUDIO_JOURNAL/$MODE/$DATE$SUFFIX
        TITLE="Session @ $DATE"
    fi

    sox --ignore-length "$1" "$OUT" $POST_FILTERS
    METADATA=$(tag "$OUT" "$DATE" "$TITLE")
    if [[ ! -v DRY_RUN ]]; then
        save_to_secondary "$OUT"
        save_to_secondary "$METADATA"
        upload "$OUT"
        upload "$METADATA"
    fi
}

get_tag() {
    id3v2 --list "$2" | grep "^$1" | sed 's/^'"$1"'[^:]*: \(.*\)$/\1/'
}

publish() {
    TITLE=$2
    if [ -z "$TITLE" ]; then
        TITLE=$(get_tag TIT2 "$1")
    fi

    DATE=$(get_tag TRDA "$1")
    if [ -z "$DATE" ]; then
        DATE=$(date -Is)
    fi

    POST_FILTERS= postprocess "$1" "$DATE" "$TITLE"
}

playback() {
    $PLAYER "$1"
}

list() {
    export -f url
    s3cmd ls -l s3://$BUCKET/$PREFIX | awk '{ print $6 }' | grep "$SUFFIX$" | sort -r | while read f; do
        url "$(basename "$f")"
    done
}
