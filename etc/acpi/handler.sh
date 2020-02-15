#!/bin/bash

case "$1" in
    *)
        logger "ACPI group/action undefined: $1 / $2"
        ;;
esac
