#!/bin/sh

# based on: https://wiki.archlinux.org/index.php/udisks#udevadm_monitor

path_to_field() {
    udevadm info -p "$2" | awk -v FS== '/'$1'/ {print $2}'
}

is_whitelisted() {
    grep --quiet "$1" ~/.automount.whitelist
}

stdbuf -oL -- udevadm monitor --udev -s block | while read -r -- _ _ EVENT DEVPATH _; do
    if [ "$EVENT" = add ]; then
        UUID=$(path_to_field ID_PART_ENTRY_UUID "$DEVPATH")
        if [ -n "$UUID" ] && is_whitelisted "$UUID"; then
            DEVNAME=$(path_to_field ID_PART_ENTRY_NAME "$DEVPATH")
            NAME=$(path_to_field ID_PART_ENTRY_NAME "$DEVPATH")
            DEVNAME=$(path_to_field DEVNAME "$DEVPATH")
            udisksctl mount --block-device "$DEVNAME" --no-user-interaction
        fi
    fi
done
