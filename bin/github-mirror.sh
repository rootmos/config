#!/bin/bash

set -o errexit

TMP=$(mktemp -d -t github-mirror.XXXX)
trap "{ rm -rf \"$TMP\"; }" EXIT

# Authorization using personal access token:
# - https://developer.github.com/v3/auth/#via-oauth-tokens
# - https://github.com/settings/tokens
AUTH=$(cat "$HOME/.github-personal-access-token")
TARGET="/mnt/volume_fra1_01/github"
BASEURL=https://api.github.com

function req_wo() {
    curl --fail --silent --user "$AUTH" -X "$1" \
        --output "$3"  --dump-header "$4" "$2"
}

function req() {
    curl --fail --silent --user "$AUTH" -X "$1" "$2"
}

function next () {
    grep "^Link" "$1" | grep next | sed 's/.*\s\+<\(\S\+\)>\s*;\s*rel\s*=\s*"next".*/\1/'
}

function paginate {
    i=1
    NEXT_URL="$2"
    while [ -n "$NEXT_URL" ]; do
        req_wo "$1" "$NEXT_URL" $3/req$i.body $3/req$i.headers
        NEXT_URL=$(next "$3"/req$i.headers)
        i=$[$i+1]
    done
}

REPOS_URL=$(req GET "$BASEURL" | jq -r .current_user_repositories_url | sed 's/{[^}]*}//')

mkdir "$TMP/repos_reqs"
paginate GET "$REPOS_URL?affiliation=owner" "$TMP/repos_reqs"

for r in $TMP/repos_reqs/*.body; do
    jq -r '.[] | [.clone_url, .full_name] | @tsv' "$r" >> "$TMP/list"
done

[ ! -d "$TARGET" ] && mkdir -p "$TARGET"

function authorized_url {
    echo "$1" | sed "s,https://,https://$AUTH@,"
}

cat "$TMP/list" | while read url name; do
    if [ -d "$TARGET/$name" ]; then (
        cd "$TARGET/$name"
        git fetch
    ) else
        git clone --bare $(authorized_url "$url") "$TARGET/$name"
    fi
done
