#!/usr/bin/env bash

readonly PREFIX="Vol:"
readonly VOLUME="$(amixer sget Master | grep "dB.*on" | cut -d ' ' -f 6 | tr -d "[]%")"

if [[ -n "${VOLUME}" ]]; then
  echo -n "${PREFIX} ${VOLUME}%"
else
  echo -n "${PREFIX} OFF"
fi
