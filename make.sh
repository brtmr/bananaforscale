#!/bin/bash

trap "exit" INT

while true; do
    ag -l | entr -rcd make banana
done
