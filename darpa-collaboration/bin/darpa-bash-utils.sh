#!/bin/bash

# --- defines
VALID_CORPORA="C-D1/recognition C-E1/recognition C-E1/description"
VALID_MOVIE_EXT="mov avi mp4"
DARPA_BIN_HOME="$HOME/darpa-collaboration/ideas/$(architecture-path)"
DSCI="$DARPA_BIN_HOME/dsci"
VIDEO_INFO="$HOME/darpa-collaboration/bin/darpa-video-info.sh"
[ "$USERNAME" = "" ] && USERNAME="$USER"
[ "$USERNAME" = "" ] && USERNAME="$(basename $HOME)"

# Directories
DARPA_DOCUMENTATION_D="$HOME/darpa-collaboration/documentation"
BASE_WORKING_D="$HOME/video-datasets"
VALID_BASE_WORKING_D="/aux/$USERNAME"
SERVER_LOCATION_D="/aux/qobi/video-datasets"
BASE_MODELS_D="C-D1/voc4-models"

CUDA_MODELS_D="CUDA-1.1"
STARCC_MODELS_D=""

# Files
CORPORA_SERVER_F="$DARPA_DOCUMENTATION_D/darpa-corpora-server.text"
SOURCE_TMP_F="$(mktemp /tmp/$EXEC-XXXXXXXXXX)"
PCA_F="felz_pca_coeff.csv"

# --- good citizen
trap source_clean_up EXIT
source_clean_up()
{
    rm -rf $SOURCE_TMP_F
}

# ---------------------------------------------------------------------------- Functions
# --- remove a file extension
extensionless()
{
    FILENAME="$1"
    EXT="$(echo "$FILENAME" | awk -F . '{if (NF>1) { print $NF }}')"
    EXTENSIONLESS="$FILENAME"
    (( ${#EXT} > 0 )) && EXTENSIONLESS="${FILENAME:0:$(expr ${#FILENAME} -  ${#EXT} - 1)}"
    echo "$EXTENSIONLESS"
}

# --- Full path to passed file
abs_path()
{
    FILENAME="$1"
    echo "$(cd "$(dirname "$FILENAME")"; pwd)/$(basename "$FILENAME")"
}

# --- Tests if a number is a float
is_float()
{
    NUMBER="$1"
    [[ "$NUMBER" =~ ^-?(([0-9]+([.][0-9]*)?)|([.][0-9]+))$ ]] && return 0
    return 1
}

# --- The number of processors on the machine
n_processors()
{
    cat /proc/cpuinfo | grep processor | wc -l
}

print_video_specs()
{
    VIDEO="$1"
    
    # Read data from darpa-corpora-server.text
    cat $CORPORA_SERVER_F | egrep "\\s$VIDEO\\s" | awk '{ print $1, " ", $3 }' > $SOURCE_TMP_F
    MAIN_CORPUS="$(cat "$SOURCE_TMP_F" | head -n 1 | awk '{ print $1 }')"
    SERVER="$(cat "$SOURCE_TMP_F" | head -n 1 | awk '{ print $2 }')"
    ALL_CORPORA="$(cat "$SOURCE_TMP_F" | awk '{ print $1 }')"

    # Make sure we are really reading a known video
    [ "$MAIN_CORPUS" = "" ] && echo "Unknown video: $VIDEO" 1>&2 && return 1 # Failure

    # Read annotations
    ANNOTATION_F="$(echo "$DARPA_DOCUMENTATION_D/$(echo "$MAIN_CORPUS" | sed s,/,-,g)-annotations.csv")"
    ANNOTATION_LINE=
    [ -e "$ANNOTATION_F" ] && ANNOTATION_LINE="$(cat "$ANNOTATION_F" | grep "$VIDEO" | sed 's/^[^,]*,//')"
    ! [ -e "$ANNOTATION_F" ] && ANNOTATION_F="(null)"

    MODELS="$(echo "$ANNOTATION_LINE" | sed 's/,/\n/g' | egrep -v "^((no)|(yes)|([0-9]+)|([a-zA-Z]))$" | sed ':a;N;$!ba;s/\n/ /g')"

    HAS_PERSON_MODEL=0
    echo "$MODELS" | grep -q person && HAS_PERSON_MODEL=1

    RUN_MODELS="$MODELS"
    (( HAS_PERSON_MODEL == 1 )) && RUN_MODELS="$MODELS person-crouch person-down"
    
    MOVIE_FILE="$(basename "$(calc_movie_extension "/net/$SERVER$SERVER_LOCATION_D/$MAIN_CORPUS/$VIDEO")")"

    cat <<EOF

VIDEO: $VIDEO
SERVER: $SERVER
MAIN_CORPUS: $MAIN_CORPUS
MOVIE_FILE: $MOVIE_FILE
ALL_CORPORA: $ALL_CORPORA
ANNOTATION_FILE: $ANNOTATION_F
ANNOTATION_LINE: $ANNOTATION_LINE
MODELS: $MODELS
HAS_PERSON_MODEL: $HAS_PERSON_MODEL

# The person model always implies running person-crouch and person-down as well
RUN_MODELS: $RUN_MODELS

# Notes: 
Original files are stored (on $SERVER) at:
$SERVER_LOCATION_D/$MAIN_CORPUS/$VIDEO

These files should be placed in your working directory:
~/video-datasets/$MAIN_CORPUS/$VIDEO
     
Where ~/video-datasets is a symlink to a folder in /aux/\$USERNAME

EOF
}

# --- Allows you to override the working directory used
set_working_directory()
{
    (( $# > 0 )) && BASE_WORKING_D="$1"
}

# --- Returns and server of video
lookup_server()
{
    VIDEO="$1"
    $VIDEO_INFO $VIDEO --key SERVER
}

# --- Returns the corpus for a video
lookup_corpus()
{
    VIDEO="$1"
    $VIDEO_INFO $VIDEO --key MAIN_CORPUS
}

# --- Returns the movie filename for video
lookup_movie_filename()
{
    VIDEO="$1"
    $VIDEO_INFO $VIDEO --key MOVIE_FILE
}

# --- Downsampled videos have a specifying name
downsampled_video_base()
{
    VIDEO="$1"
    WIDTH="$2"
    HEIGHT="$3"
    FPS="$4"
    echo "${VIDEO}${WIDTH}x${HEIGHT}@${FPS}"
}

# --- The name of the boxes filename for the specified model
boxes_fullpath_filename()
{
    VIDEO="$1"
    MODEL="$2"
    VIDEOD="$VIDEO"
    if (( $# > 2 )) ; then
	VIDEOD="$(downsampled_video_base "$VIDEO" "$3" "$4" "$5")"
    fi
    echo "$BASE_WORKING_D/$(lookup_corpus "$VIDEO")/$VIDEOD/voc4-$MODEL.boxes.zip"
}

# --- Tests against VALID_CORPORA (defined above)
is_valid_corpus()
{
    CORPUS="$1"
    for C in $VALID_CORPORA ; do
	  [ "$C" = "$CORPUS" ] && return 0
    done
    return 1 # i.e.: invalid
}

# --- Gives the movie file with proper extension
calc_movie_extension()
{
    PATH="$1"
    for EXT in $VALID_MOVIE_EXT ; do
	  [ -e "$PATH.$EXT" ] && echo "$PATH.$EXT" && return 0
    done
    echo ""
    return 1
}

# --- gets the movie file path, lazily fetching it if required
lazy_movie_path()
{
    VIDEO="$1"
    if (( $# > 1 )) ; then
	WIDTH="$2"
	HEIGHT="$3"
	FPS="$4"
    fi

    MOVIE_FILE="$(lookup_movie_filename "$VIDEO")"
    CORPUS="$(lookup_corpus "$VIDEO")"

    # Make sure movie exists
    PATHED_MOVIE="$BASE_WORKING_D/$CORPUS/$MOVIE_FILE"
    if ! [ -e "$PATHED_MOVIE" ] ; then
	SERVER="$(lookup_server $VIDEO)"		
	scp "$SERVER:$SERVER_LOCATION_D/$CORPUS/$MOVIE_FILE" "$PATHED_MOVIE"
    fi
    
    ! [ -e "$PATHED_MOVIE" ] && return 1 # failure
    
    # Make sure downsampled version exists...
    if (( $# > 1 )) ; then
	DOWNSAMPLED_MOVIE="$(downsampled_video_base $VIDEO $WIDTH $HEIGHT $FPS).avi"
	DOWNSAMPLED_MOVIE_PATH="$BASE_WORKING_D/$CORPUS/$DOWNSAMPLED_MOVIE"
	if ! [ -e "$DOWNSAMPLED_MOVIE_PATH" ] ; then
	    ffmpeg -i "$PATHED_MOVIE" -s ${WIDTH}x${HEIGHT} -r $FPS "$DOWNSAMPLED_MOVIE_PATH" 1>/dev/null 2>&1
            ! [ -e "$DOWNSAMPLED_MOVIE_PATH" ] && return 1 # failure
	fi
	PATHED_MOVIE="$DOWNSAMPLED_MOVIE_PATH"
    fi
    
    echo "$PATHED_MOVIE" && return 0
}

# -- The annotated models for a given movie
annotated_models()
{
    VIDEO="$1"
    $VIDEO_INFO $VIDEO --key "MODELS"
}

# -- The annotated models for a given movie, that are run
annotated_run_models()
{
    VIDEO="$1"
    $VIDEO_INFO $VIDEO --key "RUN_MODELS"
}

# -- Copies across the models directory if needed
ensure_models_directory()
{
    BASE="$BASE_WORKING_D/$BASE_MODELS_D"
    ! [ -d "$BASE" ] && mkdir -p "$BASE" && cp -r "/net/arivu/$SERVER_LOCATION_D/$BASE_MODELS_D" "$(dirname $BASE)"
}

# -- The full path do a cuda model file
cuda_model_filename()
{
    MODEL="$1"
    CUDA_MD="$BASE_WORKING_D/$BASE_MODELS_D/$CUDA_MODELS_D"
    ! [ -d "$CUDA_MD" ] && echo "Failed to find cuda-models-directory: $CUDA_MD" 1>&2 && return 1
    echo "$CUDA_MD/$MODEL.irobot-felz-model"
}

# -- The full path to the pca file
get_pca_filename()
{
    CUDA_MD="$BASE_WORKING_D/$BASE_MODELS_D/$CUDA_MODELS_D"
    ! [ -e "$CUDA_MD" ] && echo "Failed to find cuda-models-directory: $CUDA_MD" 1>&2 && return 1
    ! [ -e "$CUDA_MD/$PCA_F" ] && echo "Failed to locate pca file at $CUDA_MD/$PCA_F, aborting..." 1>&2 && return 1
    echo "$CUDA_MD/$PCA_F"
    return 0
}

# -- Full path to the star-cascade models
get_starcc_model_path()
{
    STARCC_D="$BASE_WORKING_D/$BASE_MODELS_D/$STARCC_MODELS_D"
    ! [ -e "$STARCC_D" ] && echo "Failed to find star-cc-models-directory: $STARCC_D" 1>&2 && return 1
    echo $STARCC_D
}

# -- Gets the full pathed model filenames for the passed video (as per annotation)
get_cuda_model_filenames()
{
    MODEL_FILENAMES=""
    for M in "$@" ; do
	FILENAME="$(cuda_model_filename "$M")"
	[ -e $FILENAME ] && MODEL_FILENAMES="$FILENAME $MODEL_FILENAMES"
	! [ -e $FILENAME ] && echo "Warning, failed to find cuda model for '$M'" 1>&2
    done
    echo "$MODEL_FILENAMES"
}

# -- Makes sure that the working directory exists, and is within /aux/$USERNAME
test_working_directory()
{
    # -- Is ~/video-datasets looking okay?
    RESOLVED="$BASE_WORKING_D"
    [ -L "$BASE_WORKING_D" ] && RESOLVED="$(ls -l "$BASE_WORKING_D" | awk '{ print $11 }')"
    
    [ "${RESOLVED:0:${#VALID_BASE_WORKING_D}}" != "$VALID_BASE_WORKING_D" ] && echo "$BASE_WORKING_D does not point to a child folder of $VALID_BASE_WORKING_D, aborting" 1>&2 && return 1
    ! [ -d "$RESOLVED" ] &&  echo "$RESOLVED, does not exist" 1>&2 && return 1
    [ "${RESOLVED:0:${#SERVER_LOCATION_D}}" = "$SERVER_LOCATION_D" ] && echo "Never allowed to use $SERVER_LOCATION_D as the working directory" 1>&2 && return 1

    return 0
}

# -- Make sure stuff exists
environment_is_sane()
{
    (( $# != 1 )) && echo "Must call environment_is_sane \$VIDEO" 1>&2 && return 1
    
    # -- Test the environment is sane for the passed video
    VIDEO="$1"

    # -- Regenerate the cache file, checking that video exists
    ! $VIDEO_INFO --recache "$VIDEO" 1>/dev/null && echo "Aborting..." 1>&2 && return 1

    # -- Is ~/video-datasets looking okay?
    ! test_working_directory && echo "Aborting..." && return 1

    # -- What is the videos server and corpus?
    CORPUS="$(lookup_corpus "$VIDEO")"
    
    # -- Make sure we have ~/video-datasets/$CORPUS/$VIDEO
    mkdir -p $BASE_WORKING_D/$CORPUS/$VIDEO
    
    # -- Make sure models exist
    ensure_models_directory

    # -- Look for movie file, also tests if CORPUS/VIDEO combination is valid
    MOVIEPATH=$(lazy_movie_path $VIDEO)
    ! [ -e "$MOVIEPATH" ] && echo "Failed to find movie file for $CORPUS/$VIDEO, aborting" 1>&2 && return 1
    
    # -- Make sure dsci and darpa-wrap are on path
    #[ ! -x "$DSCI" ] && echo "Failed to locate /executable/ dsci at $DSCI, aborting..." 1>&2 && return 1
    [ "$(which darpa-wrap)" = "" ] && echo "Failed to locate darpa-wrap on path, aborting..." 1>&2 && return 1
    return 0
}

# -- Script to cleanly run dsci stuff
sci_exec()
{
    # There must be at least 1 argument
    [ "$#" -lt 1 ] && echo "You must specify at least one command." 1>&2 && return 1
    
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
    stdbuf -i0 -o0 -e0 nice darpa-wrap $CMD $ARGS 2>&1 >&3 3>&- \
	| grep -Ev "^\\*\\*\\*\\*\\* INITIALIZEVAR Duplicately defined symbol " >&2 3>&-

    RES=$?

    # Close file-descriptor 3
    exec 3>&-

    # Report success of operation
    return $RES
}

