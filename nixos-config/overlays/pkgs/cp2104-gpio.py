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
    print >> sys.stderr, 'Usage: ' + sys.argv[0] + ' <gpioNum> <gpioVal>'
    exit(1)

gpioNum = int(sys.argv[1])
gpioVal = int(sys.argv[2])

if not (0 <= gpioNum and gpioNum <= 3):
    print >> sys.stderr, 'fatal: <gpioNum> must be in [0..3].'
    exit(1)

if not (gpioVal == 0 or gpioVal == 1):
    print >> sys.stderr, 'fatal: <gpioVal> must be either 0 or 1.'
    exit(1)

#
# 2. Find the CP2104.
#

dev = usb.core.find(idVendor = 0x10c4, idProduct = 0xea60) # https://youtu.be/xH_y05pIDTo?t=1383
if not dev:
    print >> sys.stderr, 'fatal: could not find CP2104.'
    exit(1)

#
# 3. Set the pins.
#

dev.ctrl_transfer(bmRequestType = 0b01000001,
                      bRequest = 0xff,
                      wValue = 0x37e1,
                      wIndex = (1 << gpioNum) | ((gpioVal << gpioNum) << 8))
