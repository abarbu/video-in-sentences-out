#!/bin/bash

cd "$(dirname "$0")"

# ---------------------------------------------------- Variables
T_PHP="$(pwd)/trans-team-data.php"

DESTFILE=/dev/stdout
DARPA_DOC="$HOME/darpa-collaboration/documentation"
BERKLEYSOURCE="$DARPA_DOC/vaticlabels_C-D1_0819.tar.gz"
SRI_SOURCE="$DARPA_DOC/VerbAnnotationRelease1.0.zip"
BUFFALO_SOURCE="$DARPA_DOC/mindseye_human_annotation_may11_buffalo.tar.bz"
USC_SOURCE="$DARPA_DOC/USC ME CD-1b Object Annotations per Track.rar"

for F in $SCL_T_EXE $BERKLEYSOURCE $SRI_SOURCE $BUFFALO_SOURCE ; do
    [ ! -e "$F" ] && echo "Failed to find required file $F, aborting" && exit 1
done
[ ! -e "$USC_SOURCE" ] && echo "Failed to find required file $USC_SOURCE, aborting." && exit 1
! [ -e "$T_PHP" ] && echo "Failed to find supporting php script, $T_PHP, aborting." && exit 1

TMPD=$(mktemp /tmp/unpack-boxes-data.XXXXX)
rm -rf $TMPD
mkdir -p $TMPD

# ---------------------------------------------------- Cleanup
trap clean_up EXIT
clean_up()
{
    rm -rf $TMPD
}

# ---------------------------------------------------- Documentation
show_help()
{
    cat <<EOF

    Usage: $(basename "$0") [-h|--help] 

    Picks up UBC, SRI, SBU and USC data files (for 2010-2011 years) and
    massages the data into a simplified and unified format:

       Team Video-name Track-id Frame-no Track-name Left Top Right Bottom
 
    The data is picked up from the following files:

       UBC: $BERKLEYSOURCE
       SRI: $SRI_SOURCE
       SBU: $BUFFALO_SOURCE
       USC: $USC_SOURCE

    Output is streamed to stdout.

    Requires (uses) the file $(dirname "$0")/trans-team-data.php

    Example:

       ./$(basename "$0") | ./write-boxes.sh

EOF
}

(( $# == 1 )) && [ "$1" = "-h" ] && show_help && exit 0
(( $# == 1 )) && [ "$1" = "--help" ] && show_help && exit 0

##################################################################### USC #
cd "$TMPD"
cp "$USC_SOURCE" .
unrar x -inul "$(basename "$USC_SOURCE")"
cd "Annotations/Annotation_XML_FILES/"
ls *.xml | while read L ; do
    php "$T_PHP" "USC" "$(pwd)/$L" >> "$DESTFILE"
    (( $? != 0 )) && echo "Failed on file $L, aborting" && exit 1
done

##################################################################### Buffalo #
cd "$TMPD"
cat "$BUFFALO_SOURCE" | bunzip2 -dc | tar xf -
cd mindseye_human_annotation_may11_buffalo/human_annotation/bounding_box_data
ls *.csv | while read L ; do
    php "$T_PHP" "SBU" "$(pwd)/$L" >> "$DESTFILE"
    (( $? != 0 )) && echo "Failed on file $L, aborting" && exit 1
done

##################################################################### SRI #
cd "$TMPD"
cp "$SRI_SOURCE" .
unzip -qq "$SRI_SOURCE"
cd "VerbAnnotationRelease1.0"
unzip -qq "Vigil_Mindseye_verb_annotations.zip"
cd "Vigil_Mindseye_verb_annotations"

ls *.xgtf | while read L ; do
    php "$T_PHP" "SRI" "$(pwd)/$L" >> "$DESTFILE"
    (( $? != 0 )) && echo "Failed on file $L, aborting" && exit 1;
done

##################################################################### UCB #
cd "$TMPD"
cat "$BERKLEYSOURCE" | gunzip -dc | tar xf - 
cd "vaticlabels_C-D1_0819/labels"

ls *.txt | while read L ; do
    # track-id, frame-no, track-name, left, top, right, bottom
    cat "$L" | sed 's/\"//g' | awk '{ printf("%s %s %s %d %d %d %d\n", $1, $6, ($10 == "person" ? "person" : "other"), $2*1280/720, $3*720/405, $4*1280/720, $5*720/405); }' | sed "s/^/UCB $(basename $L .txt) /" >> "$DESTFILE"
done



