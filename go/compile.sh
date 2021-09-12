#!/bin/bash

cd "$(dirname "$0")" || exit 1

exec go build -o minscheme .
