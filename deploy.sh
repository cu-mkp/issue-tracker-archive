#!/bin/bash

SITE_DIR="issue-tracker-archive/"
SITE_BRANCH="gh-pages"

bash update.sh
runhaskell build.hs

git add $SITE_DIR
git commit -m "Build site"
git push -d origin $SITE_BRANCH
git subtree push --prefix $SITE_DIR origin $SITE_BRANCH
git reset HEAD~1
