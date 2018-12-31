#!/usr/bin/env bash
set -eu -x -o pipefail

cd "$(dirname $0)"
mkdir -p dist

pushd dist
rm -rf fwoar.lisputils
git clone https://github.com/fiddlerwoaroof/fwoar.lisputils.git
rm -rf data-lens
git clone https://github.com/fiddlerwoaroof/data-lens.git
popd

export CL_SOURCE_REGISTRY="$PWD/dist//"
sbcl --no-userinit \
     --disable-debugger \
     --load ~/quicklisp/setup.lisp \
     --eval "(progn (pushnew :$1 *features*)
                    (load \"build.lisp\"))"
