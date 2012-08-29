#!/bin/bash
#
# Modeled from run-cupedro.sh


# --- defines
EXEC="$(basename "$0")"
EXEC_PATH=`cd $(dirname "$0"); pwd`
N_FEATURES=1000
PYRAMID=3
MASK_SIZE=15
TEMPLATE_SIZE=11


# -- include darpa-bash-utils.sh
source "$HOME/darpa-collaboration/bin/darpa-bash-utils.sh"


# --- help message
show_help()
{
    cat <<EOF
  Usage: $EXEC corpus video-name [num-features [pyramid [mask-size [template-size]]]]

  Default values:
     num-features  = 1000
     pyramid       = 3
     mask-size     = 15
     template-size = 11

  Looks up the passed video-name (in corpus) and does the following:
   + Makes sure appropriate folders are set up in ~/video-datasets
   + Runs all per-video-klt_v2 on the video
   + Saves a single klt video file ~/video-datasets/corpus/video-name/

   This script calls bin/per-video-klt_v2
   The ouput is placed in ~/video-datasets/corpus/video-name/klt.txt


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
(( $# >= 3 )) && N_FEATURES="$3"
(( $# >= 4 )) && PYRAMID="$4"
(( $# >= 5 )) && MASK_SIZE="$5"
(( $# >= 6 )) && TEMPLATE_SIZE="$6"


# -- Basic checks on arguments
! is_valid_corpus "$CORPUS" && \
    echo "Invalided corpus $CORPUS, " \
    "(expected one of: $VALID_CORPORA), aborting..." 1>&2 && \
    exit 1

! [ "$N_FEATURES" -eq "$N_FEATURES" ] 2> /dev/null && \
    echo "$N_FEATURES is not an integer, aborting..." 1>&2 && \
    exit 1

! [ "$PYRAMID" -eq "$PYRAMID" ] 2> /dev/null && \
    echo "$PYRAMID is not an integer, aborting..." 1>&2 && \
    exit 1

! [ "$MASK_SIZE" -eq "$MASK_SIZE" ] 2> /dev/null && \
    echo "$MASK_SIZE is not an integer, aborting..." 1>&2 && \
    exit 1

! [ "$TEMPLATE_SIZE" -eq "$TEMPLATE_SIZE" ] 2> /dev/null && \
    echo "$TEMPLATE_SIZE is not an integer, aborting..." 1>&2 && \
    exit 1

! [ -x $EXEC_PATH/bin/per-video-klt_v2 ] && \
    echo "could not find $EXEC_PATH/bin/per-video-klt_v2" 1>&2 && \
    exit 1

# -- Make sure the underlying environment is sane
! environment_is_sane $VIDEO && exit 1


# -- Ready to execute
echo "$EXEC parameters..."
echo "CORPUS = $CORPUS"
echo "MOVIE-FILE = $(basename $MOVIEPATH)"
echo "N_FEATURES = $N_FEATURES"
echo "PYRAMID = $PYRAMID"
echo "MASK_SIZE = $MASK_SIZE"
echo "TEMPLATE_SIZE = $TEMPLATE_SIZE"


cd ~/video-datasets/$CORPUS/$VIDEO/
$EXEC_PATH/bin/per-video-klt_v2			\
    --numFeatures $N_FEATURES			\
    --pyramid      $PYRAMID			\
    --maskSize     $MASK_SIZE			\
    --templateSize  $TEMPLATE_SIZE		\
    $MOVIEPATH
cd -

# darpa-wrap /home/abarbu/darpa-collaboration/ideas/x86_64-Debian-6.0.2/viterbi-tracker -scmh 6000 -t 12 -darpa ${video} /aux/qobi/video-datasets/C-D1/voc4-models 5

#  darpa-wrap ~/darpa-collaboration/ideas/x86_64-Linux-2.6.32-5-amd64/viterbi-tracker -scmh 6000 -t 12  -darpa HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2 /home/chang177/video-datasets/C-D1/voc4-models/ 5

