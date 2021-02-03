#!/bin/bash

rm -r _site/
python3 update.py
node build.js
git add _site/
git commit -m "Build site"
git push -d origin gh-pages
git subtree push --prefix _site/ origin gh-pages
git reset HEAD~1
