#!/usr/bin/env bash
set -eu -x -o pipefail

cd "$(dirname $0)"
mkdir -p dist

pushd dist
rm -rf fwoar.lisputils
git clone https://github.com/fiddlerwoaroof/fwoar.lisputils.git
popd

export CL_SOURCE_REGISTRY="$PWD/dist//"
sbcl --no-userinit \
     --load ~/quicklisp/setup.lisp \
     --load build.lisp
