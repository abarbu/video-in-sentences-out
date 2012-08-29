#!/bin/bash
#

# --- defines
EXEC="$(basename "$0")"
EXEC_PATH=`cd $(dirname "$0"); pwd -P`


# -- include darpa-bash-utils.sh
source "$HOME/darpa-collaboration/bin/darpa-bash-utils.sh"


# --- help message
show_help()
{
    cat <<EOF
  Usage: $EXEC corpus video-name 

  Looks up the passed video-name (in corpus) and does the following:
   + Makes sure appropriate folders are set up in ~/video-datasets
   + Runs all per-video-kil on the video
   + Saves a single klt video file ~/video-datasets/corpus/video-name/

   This script calls ~/darpa-collaboration/klt/klt_ffmpeg 

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

(( $# > 5 )) && \
    echo "Expected no more then 5 arguments -- type -h for help" 1>&2 && \
    exit 1

CORPUS="$1"
VIDEO="$2"

# -- Make sure the underlying environment is sane
! environment_is_sane $VIDEO && exit 1


# -- Ready to execute
echo "$EXEC parameters..."
echo "CORPUS = $CORPUS"
echo "MOVIE-FILE = $(basename $MOVIEPATH)"

#$EXEC_PATH/../../klt/klt_ffmpeg $MOVIEPATH ~/video-datasets/$CORPUS/$VIDEO/
/home/darpa-collaboration/klt/klt_ffmpeg \
   $MOVIEPATH ~/video-datasets/$CORPUS/$VIDEO/
