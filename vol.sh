#!/usr/bin/env bash

vol=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "MM" } else { print $2/10 }}' | head -n 1)

echo $vol
