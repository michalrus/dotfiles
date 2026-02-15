#!/usr/bin/env python

#
# This small util is used to write values to CP2104's GPIO pins.
#
# Cf. https://www.silabs.com/documents/public/application-notes/AN571.pdf for
# specific values used below.
#

import sys
import usb

#
# 1. Parse argv.
#

if len(sys.argv) != 3:
    print("Usage: " + sys.argv[0] + " <gpioNum> <gpioVal>", file=sys.stderr)
    sys.exit(1)

gpioNum = int(sys.argv[1])
gpioVal = int(sys.argv[2])

if not 0 <= gpioNum <= 3:
    print("fatal: <gpioNum> must be in [0..3].", file=sys.stderr)
    sys.exit(1)

if gpioVal not in (0, 1):
    print("fatal: <gpioVal> must be either 0 or 1.", file=sys.stderr)
    sys.exit(1)

#
# 2. Find the CP2104 (<https://youtu.be/xH_y05pIDTo?t=1383>).
#

dev = usb.core.find(idVendor=0x10C4, idProduct=0xEA60)
if not dev:
    print("fatal: could not find CP2104.", file=sys.stderr)
    sys.exit(1)

#
# 3. Set the pins.
#

dev.ctrl_transfer(
    bmRequestType=0b01000001,
    bRequest=0xFF,
    wValue=0x37E1,
    wIndex=(1 << gpioNum) | ((gpioVal << gpioNum) << 8),
)
