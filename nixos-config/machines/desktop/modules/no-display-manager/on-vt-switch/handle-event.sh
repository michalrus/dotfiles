#!/bin/sh

echo "switched VT from $1 to $2"
exec sleep "$2"
