#!/bin/bash
if [ -f ./.env ] ;
then
    source .env
else
    echo "error: missing .env file. Unable to source authorization token."
    exit 2
fi

RATE_RESPONSE=$(curl -s --fail \
    -H "Authorization: token $TOKEN" \
    "https://api.github.com/rate_limit")

OUTFILE_ISSUES="./_data/_issues.json"
OUTFILE_COMMENTS="./_data/_comments.json"

if [ -f $OUTFILE_ISSUES ] ;
then
    rm $OUTFILE_ISSUES
fi

if [ -f $OUTFILE_COMMENTS ] ;
then
    rm $OUTFILE_COMMENTS
fi

PER_PAGE=100
PAGES=30
CURR_PAGE=1
echo "[" >> $OUTFILE_ISSUES
while [ $CURR_PAGE -le $PAGES ] ;
do
    RESPONSE=$(curl -s --fail \
        -H "Authorization: token $TOKEN" \
        "https://api.github.com/repos/cu-mkp/m-k-manuscript-data/issues?state=all&direction=asc&sort=created&page=$CURR_PAGE&per_page=$PER_PAGE")

    echo "${RESPONSE:1: -1}" >> $OUTFILE_ISSUES
    CURR_PAGE=$((CURR_PAGE+1))
done
echo "]" >> $OUTFILE_ISSUES

# This is a terrible solution.
# Better solutions involve using a real programming language that understands JSON.
# Also it assumes <=100 comments per issue.
echo "{" >> $OUTFILE_COMMENTS
# evil sed hack
for COMMENTS_URL_LINE in $(cat $OUTFILE_ISSUES | sed -n "s/^\s*\"comments_url\": \"\(\S*\)\",$/\1/g;tp;b;:p;=");
do
    COMMENTS_URL=$(sed "${COMMENTS_URL_LINE}q;d" $OUTFILE_ISSUES | sed -n "s/^\s*\"comments_url\": \"\(\S*\)\",$/\1/p")
    ISSUE_ID_LINE=$((COMMENTS_URL_LINE+3))
    ISSUE_ID=$(sed "${ISSUE_ID_LINE}q;d" $OUTFILE_ISSUES | sed -n "s/^\s*\"id\": \(\S*\),$/\1/p")
    RESPONSE=$(curl -s --fail \
        -H "Authorization: token $TOKEN" \
        "$COMMENTS_URL?direction=asc&sort=created&per_page=100")

    echo "$ISSUE_ID:" >> $OUTFILE_COMMENTS
    echo "$RESPONSE," >> $OUTFILE_COMMENTS
done
sed -i "$d" $OUTFILE_COMMENTS
echo "]" >> $OUTFILE_COMMENTS
echo "}" >> $OUTFILE_COMMENTS
