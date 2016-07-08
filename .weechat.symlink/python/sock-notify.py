# -*- coding: utf-8 -*-
import weechat
import socket
from os.path import expanduser

SCRIPT_NAME    = 'sock-notify'
SCRIPT_AUTHOR  = 'Michal Rus'
SCRIPT_VERSION = '0.1'
SCRIPT_LICENSE = 'MIT'
SCRIPT_DESC    = 'Send broadcast command to a UNIX socket'
SCRIPT_DELIM   = ':::::::'

def notify(title, message):
  c = socket.socket( socket.AF_UNIX, socket.SOCK_STREAM )
  c.connect(expanduser("~") + "/.weechat/notify.sock")
  c.send("broadcast " + title + "\t" + message + "\r\n")
  buf = c.recv(128) # let’s wait for some bytes back; without this, the notification isn’t always broadcasted
  c.close()

def parse_cmd(data, buffer, args):
  largs = args.split(SCRIPT_DELIM, 1)
  notify(largs[0], largs[1])
  return weechat.WEECHAT_RC_OK

if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE, SCRIPT_DESC, '', ''):
  weechat.hook_command(SCRIPT_NAME, SCRIPT_DESC, '<title>' + SCRIPT_DELIM + '<message>', '', '', 'parse_cmd', '')
