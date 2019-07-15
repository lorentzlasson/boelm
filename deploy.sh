#!/bin/bash
set -e

./build.sh
git checkout gh-pages
cp dist/* .
git add *.js *.html
git commit -m 'Deploy'
git push
git checkout -
