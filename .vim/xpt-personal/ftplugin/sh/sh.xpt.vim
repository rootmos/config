XPTemplate priority=personal

XPTvar $BRif ' '
XPTvar $BRel \n
XPTvar $BRloop ' '
XPTvar $BRfun ' '
XPTvar $author 'Gustav Behm'
XPTvar $email gustav.behm@gmail.com

XPT bash
#!/bin/bash

set -o nounset -o pipefail -o errexit

`cursor^

XPT sh
#!/bin/bash

set -o nounset -o errexit

`cursor^

XPT errexit
set -o nounset -o pipefail -o errexit

XPT getopts
while getopts "`args^-" OPT; do
    case $OPT in
        `cursor^
        -) break ;;
        ?) exit 2 ;;
    esac
done
shift $((OPTIND-1))

XPT mktemp
TMP=$(mktemp -d)
trap 'rm -rf $TMP' EXIT
`cursor^

XPT xtrace
set -o xtrace

XPT script_dir
SCRIPT_DIR=$(readlink -f "$0" | xargs dirname)

XPT usage
usage() {
    cat <<EOF 1>&2
Usage: $(basename "$0")
Options:
    -h      show this message
    `cursor^
EOF
    exit "${1-0}"
}
