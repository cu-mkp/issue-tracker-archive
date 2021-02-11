#!/bin/bash
if [ -f ./.env ] ;
then
    source .env
else
    echo "error: missing .env file. Unable to source authorization token."
    exit 2
fi

RATE_RESPONSE=$(curl -s --fail -H "Authorization: token $TOKEN" "https://api.github.com/rate_limit")

OUTFILE_ISSUES="./_data/issues.json"
OUTFILE_COMMENTS="./_data/comments.json"

HEADERS_FILE="./tmp/headers"

if [ -f $OUTFILE_ISSUES ] ;
then
    rm $OUTFILE_ISSUES
fi

if [ -f $OUTFILE_COMMENTS ] ;
then
    rm $OUTFILE_COMMENTS
fi

mkdir -p "./tmp/"

if [ -f $HEADERS_FILE ] ;
then
    rm $HEADERS_FILE
fi


depaginate_and_write () {
    # Arguments:
    #   $1: initial target url
    #   $2: file to write to
    TARGET_URL=$1
    OUTFILE=$2

    DONE=0
    echo "[" >> $OUTFILE
    while [ $DONE != 1 ] ;
    do
        RESPONSE=$(curl -D "$HEADERS_FILE" -s --fail \
            -H "Authorization: token $TOKEN" \
            "$TARGET_URL")

        # Use pagination data to get next url.
        TARGET_URL=$(cat $HEADERS_FILE | sed -n "s/^link: .*<\(.*\)>; rel=\"next\".*$/\1/p")
        # No "next" in header => we're done.
        if [ -z "$TARGET_URL" ] ;
        then
            DONE=1
        fi

        echo "${RESPONSE:1: -1}" >> $OUTFILE
        echo "," >> $OUTFILE
    done
    # Remove the last ","
    sed -i '$d' $OUTFILE
    echo "]" >> $OUTFILE
}

PER_PAGE=2

depaginate_and_write "https://api.github.com/repos/cu-mkp/m-k-manuscript-data/issues?state=all&direction=asc&sort=created&page=1&per_page=$PER_PAGE" $OUTFILE_ISSUES

depaginate_and_write "https://api.github.com/repos/cu-mkp/m-k-manuscript-data/issues/comments?direction=asc&sort=created&per_page=$PER_PAGE" $OUTFILE_COMMENTS

rm -r "./tmp/"
