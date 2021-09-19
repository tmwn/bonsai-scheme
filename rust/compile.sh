#!/bin/bash

cd "$(dirname "$0")" || exit

exec cargo build