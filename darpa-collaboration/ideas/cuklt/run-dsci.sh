#!/bin/bash

# --- defines
EXEC="$(basename "$0")"
EXEC_PATH=`cd $(dirname "$0"); pwd`
KLT_BIN=~/darpa-collaboration/ideas/`architecture-path`/klt

# -- include darpa-bash-utils.sh
source "$HOME/darpa-collaboration/bin/darpa-bash-utils.sh"

# --- help message
show_help()
{
    cat <<EOF
  Usage: $EXEC corpus video-name [num-features [pyramid [mask-size [template-size]]]]

  Looks up the passed video-name (in corpus) and does the following:
   + Makes sure appropriate folders are set up in ~/video-datasets
   + Saves a single klt.zip to ~/video-datasets/corpus/video-name/

   This script calls ~/darpa-collaboration/ideas/`architecture-path`/klt
   The ouput is placed in ~/video-datasets/corpus/video-name/klt.zip

  Example:
     darpa-wrap $0 C-D1/recognition HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2

     darpa-wrap $0 C-D1/recognition Collide9_A2_C1_Act1_PARK1_FR_AFTN_47d14da3-c5af-11df-84f7-e80688cb869a

EOF
}


# --- Parse arguments
(( $# == 0 )) && \
    echo "Expected at least 1 argument -- type -h for help" 1>&2 && \
    exit 1

(( $# > 0 )) && \
    [ "$1" = "-h" ] || \
    [ "$1" = "--help" ] && \
    show_help && \
    exit 0

(( $# < 2 )) && \
    echo "Expected at least 2 arguments -- type -h for help" 1>&2 && \
    exit 1

CORPUS="$1"
VIDEO="$2"

# -- Basic checks on arguments
! is_valid_corpus "$CORPUS" && \
    echo "Invalided corpus $CORPUS, " \
    "(expected one of: $VALID_CORPORA), aborting..." 1>&2 && \
    exit 1

! [ -x $KLT_BIN ] && \
    echo "could not find $KLT_BIN" 1>&2 && \
    exit 1

# -- Make sure the underlying environment is sane
! environment_is_sane $VIDEO && exit 1


# -- Ready to execute
echo "$EXEC parameters..."
echo "CORPUS = $CORPUS"
echo "MOVIE-FILE = $(basename $MOVIEPATH)"


cd ~/video-datasets/$CORPUS/$VIDEO/
echo $KLT_BIN -darpa $VIDEO
$KLT_BIN -darpa $VIDEO
cd -

