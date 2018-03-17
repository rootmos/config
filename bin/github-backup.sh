#!/bin/bash

set -o errexit

ORIG_DIR=$PWD

ARCHIVE=github-$(date -Im).tar.gz

TMP=$(mktemp -d -t github-backup.XXXX)
trap "{ rm -rf $TMP; }" EXIT

# Authorization using personal access token:
# - https://developer.github.com/v3/auth/#via-oauth-tokens
# - https://github.com/settings/tokens
AUTH=$(pass github/backup)

BASEURL=https://api.github.com

function req_wo() {
    curl --output $3 --user $AUTH -X $1 --dump-header $4 $2
}

function body {
    cat $1 | dos2unix | sed '1,/^$/d'
}

function next () {
    grep "^Link" $1 | grep next | sed 's/.*\s\+<\(\S\+\)>\s*;\s*rel\s*=\s*"next".*/\1/'
}

function paginate {
    i=1
    NEXT_URL=$2
    while [ -n "$NEXT_URL" ]; do
        req_wo $1 $NEXT_URL $3/req$i.body $3/req$i.headers
        NEXT_URL=$(next $3/req$i.headers)
        i=$[$i+1]
    done
}

REPOS_URL=$(curl $BASEURL | jq -r .current_user_repositories_url | sed 's/{[^}]*}//')

mkdir $TMP/repos_reqs
paginate GET $REPOS_URL"?affiliation=owner" $TMP/repos_reqs

for r in $TMP/repos_reqs/*.body; do
    jq -r .[].ssh_url $r >> $TMP/list
done

mkdir $TMP/repos
cd $TMP/repos

for r in $(cat $TMP/list); do
    git clone --bare $r
done

cd ..
tar czf $ARCHIVE --force-local repos

if [ $# = 0 ]; then
    cp -v $ARCHIVE $ORIG_DIR
else
    cp -v $ARCHIVE $@
fi
