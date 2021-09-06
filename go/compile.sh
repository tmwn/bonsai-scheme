#!/bin/bash

cd "$(dirname "$0")"

exec go build -o minscheme .

