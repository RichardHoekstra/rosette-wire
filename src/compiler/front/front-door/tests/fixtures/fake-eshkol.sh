#!/bin/sh
# Deterministic orchestration fixture; it is not an Eshkol semantics oracle.
set -eu

mode=${ROSETTE_FAKE_ESHKOL_MODE:-ok}
program_id=${ROSETTE_FAKE_ESHKOL_PROGRAM_ID:-missing}
value=${ROSETTE_FAKE_ESHKOL_VALUE:-0}

for argument in "$@"; do
  if [ "$argument" = "--version" ]; then
    if [ "$mode" = "availability-fail" ]; then
      exit 2
    fi
    printf '%s\n' 'fake-eshkol 1'
    exit 0
  fi
done

run_source=
output=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --run)
      shift
      run_source=${1:-}
      ;;
    --output)
      shift
      output=${1:-}
      ;;
  esac
  shift
done

if [ -n "$run_source" ]; then
  case "$mode" in
    jit-fail) exit 3 ;;
    malformed-jit)
      printf '%s\n' 'ROSETTE-FRONT-DOOR-ESHKOL-RESULT (:bad t)'
      ;;
    *)
      printf 'ROSETTE-FRONT-DOOR-ESHKOL-RESULT (:schema :rosette-front-door-eshkol-result/v1 :ok t :program-id "%s" :value %s)\n' \
        "$program_id" "$value"
      ;;
  esac
  exit 0
fi

if [ "$mode" = "compile-fail" ]; then
  exit 4
fi
if [ "$mode" = "missing-artifact" ]; then
  exit 0
fi
if [ -z "$output" ]; then
  exit 5
fi

if [ -n "${ROSETTE_FAKE_ESHKOL_OUTPUT_LOG:-}" ]; then
  printf '%s\n' "$output" >> "$ROSETTE_FAKE_ESHKOL_OUTPUT_LOG"
fi

if [ "$mode" = "malformed-aot" ]; then
  printf '%s\n' '#!/bin/sh' \
    "printf '%s\\n' 'ROSETTE-FRONT-DOOR-ESHKOL-RESULT (:bad t)'" > "$output"
else
  printf '%s\n' '#!/bin/sh' \
    "printf '%s\\n' 'ROSETTE-FRONT-DOOR-ESHKOL-RESULT (:schema :rosette-front-door-eshkol-result/v1 :ok t :program-id \"$program_id\" :value $value)'" \
    > "$output"
fi
chmod 700 "$output"
