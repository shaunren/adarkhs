#!/usr/bin/env bash

# Compiling with ghcjs:
stack haddock --stack-yaml=stack-ghcjs.yaml --ghc-options '-j'

# Moving the generated files to the js folder:
mkdir -p out
cp -r $(stack path --local-install-root --stack-yaml=stack-ghcjs.yaml)/bin/adarkhs.jsexe/all.js{,.externs} out/

# Minifying all.js file using the closure compiler:
cd out
ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=node --externs=all.js.externs > all.min.js
