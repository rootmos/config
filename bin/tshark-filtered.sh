#!/bin/sh

unset FILTERS
EXTRA_FILTERS=
INTERFACE_OPT=
while getopts "f:i:asSn" opt; do
    case $opt in
        f) FILTERS="-Y $OPTARG" ;;
        a) EXTRA_FILTERS="$EXTRA_FILTERS and !arp" ;;
        S) EXTRA_FILTERS="$EXTRA_FILTERS and !smb2" ;;
        s) EXTRA_FILTERS="$EXTRA_FILTERS and !ssl" ;;
        n) EXTRA_FILTERS="$EXTRA_FILTERS and !ntp" ;;
        n) EXTRA_FILTERS="$EXTRA_FILTERS and !ntp" ;;
        i) INTERFACE_OPT="-i $OPTARG" ;;
        \?) echo "Invalid option: -$OPTARG" >&2
            exit 2 ;;
    esac
done
shift $((OPTIND-1))

R=$(dig +short ip.rootmos.io)
FILTERS=${FILTERS--Y "!(ip.addr == $R) and !(tcp.flags.ack && tcp.len <= 1) $EXTRA_FILTERS"}
exec tshark $INTERFACE_OPT "$FILTERS"
