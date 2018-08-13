#!/bin/sh

ghci -XOverloadedStrings -freverse-errors -isrc:app/common:app/vkq:app/runhaskell "$@" | python3 -c '
import sys
import re
import signal

def sigint_handler(signum, frame):
    print ("Stop pressing the CTRL+C!")

signal.signal(signal.SIGINT, sigint_handler)

def tr(match):
    s = match.group(1)
    try:
        return chr(int(s))
    except ValueError:
        return s

for line in sys.stdin:
    sys.stdout.write(re.sub(r"\\([0-9]{4})", tr, line))
'
