#!/bin/bash

set -x
set -u

# download and compile ranger
git clone --depth 1 https://github.com/imbs-hl/ranger.git
cd ranger/cpp_version
mkdir build
cd build
cmake ../
make -j `getconf _NPROCESSORS_ONLN`
cp ranger `opam config var bin`/ml_rf_ranger
