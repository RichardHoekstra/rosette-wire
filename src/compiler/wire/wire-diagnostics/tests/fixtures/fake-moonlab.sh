#!/bin/sh
set -eu

mode="${ROSETTE_FAKE_MOONLAB_MODE:-ok}"
[ "$mode" != "unavailable" ] || exit 69

abi="0 6 0"
[ "$mode" != "old-abi" ] || abi="0 5 0"

IFS= read -r protocol
if [ "$protocol" = "ROSETTE-MOONLAB-SURFACES/2" ]; then
  read -r measurement_key measurement_draw
  read -r channel_key channel_parameter
  read -r qgt_key qgt_mass
  read -r gradient_key gradient_a gradient_b
  read -r end
  [ "$measurement_key" = "MEASUREMENT" ]
  [ "$channel_key" = "CHANNEL" ]
  [ "$qgt_key" = "QGT-MASS" ]
  [ "$gradient_key" = "GRADIENT" ]
  [ "$end" = "END" ]
  case "$qgt_mass" in
    -3*|3*) qgt_expected=0 ;;
    -1*) qgt_expected=1 ;;
    1*) qgt_expected=-1 ;;
    *) exit 65 ;;
  esac
  qgt_observed="$qgt_expected.00000000000000000e+00"
  qgt_reference="$qgt_expected.00000000000000000e+00"
  [ "$mode" != "surface-qgt-mismatch" ] || qgt_observed=9.00000000000000000e+00
  [ "$mode" != "surface-qgt-reference-mismatch" ] || qgt_reference=9.00000000000000000e+00
  gradient_residual=0.00000000000000000e+00
  [ "$mode" != "surface-gradient-mismatch" ] || gradient_residual=1.00000000000000000e+00
  gpu_kind=UNAVAILABLE
  gpu_status=-5
  [ "$mode" != "surface-gpu-fail" ] || { gpu_kind=FAIL; gpu_status=-9; }
  printf '%s\n' 'ROSETTE-MOONLAB-SURFACES/2'
  printf 'ABI %s\n' "$abi"
  printf '%s\n' \
    'MEASUREMENT 5.00000000000000000e-01 0 0.00000000000000000e+00 -1' \
    'CHANNELS 2.22044604925031308e-16 -1.00000000000000000e+00'
  printf 'QGT 0 1024 %s %s 1\n' "$qgt_observed" "$qgt_expected"
  printf 'QGT-REFERENCE %s %s\n' "$qgt_expected" "$qgt_reference"
  printf 'GRADIENT 0 1.00000000000000000e+00 -5.00000000000000000e-01 1.00000000000000000e+00 -5.00000000000000000e-01 %s -1 -2 1\n' "$gradient_residual"
  printf '%s\n' 'OWNERSHIP 1 -1 1'
  printf 'GPU %s %s 0.00000000000000000e+00\n' "$gpu_kind" "$gpu_status"
  printf '%s\n' 'END'
  exit 0
fi

[ "$protocol" = "ROSETTE-MOONLAB/1" ] || exit 65
cat >/dev/null
printf '%s\n' 'ROSETTE-MOONLAB/1'
printf 'ABI %s\n' "$abi"
printf '%s\n' 'OWNERSHIP 1' 'ERROR-PATH 1' 'STATE 4'
if [ "$mode" = "mismatch" ]; then
  printf '%s\n' \
    'AMP 7.07106781186547573e-01 0.00000000000000000e+00' \
    'AMP 7.07106781186547573e-01 0.00000000000000000e+00' \
    'AMP 0.00000000000000000e+00 0.00000000000000000e+00' \
    'AMP 0.00000000000000000e+00 0.00000000000000000e+00'
else
  printf '%s\n' \
    'AMP 7.07106781186547573e-01 0.00000000000000000e+00' \
    'AMP 0.00000000000000000e+00 0.00000000000000000e+00' \
    'AMP 0.00000000000000000e+00 0.00000000000000000e+00' \
    'AMP 7.07106781186547573e-01 0.00000000000000000e+00'
fi
printf '%s\n' 'END'
