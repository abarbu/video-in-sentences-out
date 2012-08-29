#!/bin/bash

cd "$(dirname "$0")"

TMPD=$(mktemp /tmp/unpack-boxes-data.XXXXX)
rm -rf $TMPD
mkdir -p $TMPD

# ----------------------------------------------------------------- Variables
MANIFEST=$HOME/darpa-collaboration/documentation/darpa-corpora-server.text
SOURCEF=$TMPD/input-data.text
VERBSF=$TMPD/verb-senses.text
SPLITD=$TMPD/splits
UNIQSF=$TMPD/uniques.text
FRAMES=$TMPD/frames.text
GROUPS="SRI UCB USC SBU"
DARPA_BUILD_DIR="$HOME/darpa-collaboration/ideas/$(architecture-path)"
DSCI="$DARPA_BUILD_DIR/dsci"

mkdir -p "$SPLITD"

# ----------------------------------------------------------------- Cleanup
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

    Reads input (from stdin) in the format produced by ./reprocess.sh.
    The data is processed to produce valid track files which are written
    to ~/video-datasets.

    Example:

       ./reprocess.sh | ./write-boxes.sh

EOF
}

# ---------------------------------------------------- Warp
warp_darpa()
{
    CMD="$1"                          # The command to execute
    shift
    ARGS="$@"                         # Arguments to pass said command

    # Open file-descriptor 3
    exec 3>/dev/stdout

    # Route 2>&1, (stderr goes to 1, which is pumped through the pipe to grep)
    # Route >&3,  (stdout goes to 3, which goes directly to /dev/stdout)
    # 3>&- closes file-descriptor 3 for the grep command /only/
    # Finally, at the end of the grep command we route its output back to &2 (stderr)
    # The trailing 3>&- is redundant, but would be relavant for further chained commands
    nice darpa-wrap $CMD $ARGS 2>&1 >&3 3>&- \
	| grep -Ev "^\\*\\*\\*\\*\\* INITIALIZEVAR Duplicately defined symbol " >&2 3>&-

    # Close file-descriptor 3
    exec 3>&-
}

(( $# == 1 )) && [ "$1" = "-h" ] && show_help && exit 0
(( $# == 1 )) && [ "$1" = "--help" ] && show_help && exit 0

[ "" = "$(which darpa-wrap)" ] && echo "Failed to find darpa-wrap on path, aborting." 1>&2 && exit 1
# ! [ -e "$DSCI" ] && echo "Failed to find dsci at $DSCI, aborting." 1>&2 && exit 1

# Save standard input into SOURCEF
cat /dev/stdin > $SOURCEF

# ----------------------------------------------------------------- Utilities
get_dest_dir()
{
    VIDEONAME="$1"
    LINE="$(cat $MANIFEST | grep $VIDEONAME | head -n 1 | awk '{ print $1, " ", $3 }')"
    
    if [ "$LINE" == "" ] ; then
	echo "Failed to find video $VIDEONAME in $MANIFEST" 1>&2
	exit 1
    fi

    CORPUS="$(echo "$LINE" | awk '{ print $1 }')"
    
    echo "$HOME/video-datasets/$CORPUS/$VIDEONAME"
}

process_line()
{
    RAWDATA="$1"
    TEAM=$2
    VIDEONAME=$3
    TRACKID=$4
    TRACKNAME=$5

    shift
    echo -n "Processing $* - "

    DESTD="$(get_dest_dir "$VIDEONAME")"
    mkdir -p "$DESTD"

    DESTF="$DESTD/${TEAM}-${TRACKNAME}-${TRACKID}.smooth-tracked-box"
    VLEN="$($HOME/darpa-collaboration/bin/darpa-video-length.sh $VIDEONAME)"
    ! [ "$VLEN" -eq "$VLEN" 2>/dev/null ] && echo "could not calculate VIDEO-LENGTH, skipping" && return
    NFRAMES=$(expr $VLEN - 1)

    SEARCH="$TEAM $VIDEONAME $TRACKID "

    rm -rf "$DESTF"
    mkdir -p "$(dirname "$DESTF")"

    cat "$RAWDATA" | grep "$SEARCH" | grep " $TRACKNAME " | sort -n -k 4 | awk 'BEGIN { fn=0; blankln = "-1 -1 -1 -1 -1 0 -1 padding"; }; { frameno = $4; while(++fn < frameno) printf("1\n%s\n", blankln); printf("1\n%d %d %d %d 0 0 0 %s\n", $6, $7, $8, $9, $5); }' > $DESTF
    
    W="$(echo "line-count = $LINECOUNT, nframes = $NFRAMES")"
    LINECOUNT=$(expr $(cat $DESTF | wc -l) / 2)
    while (( $LINECOUNT < $NFRAMES )) ; do
	echo "1" >> $DESTF
	echo "-1 -1 -1 -1 -1 0 -1 padding" >> $DESTF
	LINECOUNT=$(expr $LINECOUNT + 1)
    done

    echo "done."
}

# Get all the different possible verb
echo -n "Calculating unique verb-senses - "
cat "$SOURCEF" | awk '{ print $2 }' | awk -F_ '{ print $1 }' | sort | uniq > $VERBSF
echo "done."

# Split the input file
cat $VERBSF | while read VERBS ; do
    SPLITF="$SPLITD/$VERBS"
    SPLITUNIQ="$SPLITD/${VERBS}.uniq"
    cat "$SOURCEF" | egrep "^... ${VERBS}_" > "$SPLITF"
    cat "$SPLITF" | awk '{ print $1, " ", $2, " ", $3, " ", $5 }' | sed 's/[ ]\+/ /g' | sort | uniq > $SPLITUNIQ
    cat $SPLITUNIQ | while read L ; do
	process_line $SPLITF $L 
    done
done

exit 0


