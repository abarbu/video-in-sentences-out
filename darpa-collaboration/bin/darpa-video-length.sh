#!/bin/bash

EXEC="$(basename "$0")"
CACHE_D="/tmp/$EXEC-$(basename $HOME)-cache"
DO_CLEAN=0
RECACHE=0

# --- help message
show_help()
{
    cat <<EOF

   Usage: $EXEC [[--recache] VIDEO-NAME [--clean] [--help]

      --recache   Forces $EXEC to rebuild the cache file for the passed video-name
      --clean     Deletes /any/ cached files on exit

   Example:

      # Displays video length
      $EXEC HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2

      # Deletes any cached files and exists
      $EXEC --clean

EOF
}

(( $# == 0 )) && echo "Expected at least 1 argument -- displaying help and exiting," 1>&2 && show_help && exit 1
(( $# > 0 )) && [ "$1" = "-h" ] || [ "$1" = "--help" ] && show_help && exit 0

VIDEO=
KEY=
while (( $# > 0 )) ; do
   ARG="$1"
   if [ "$ARG" == "--clean" ] ; then DO_CLEAN=1 
   elif [ "$ARG" == "--recache" ] ; then RECACHE=1
   else VIDEO="$ARG"
   fi
   shift
done

# -- Are arguments sane?
[ "$VIDEO" == "" ] && (( $DO_CLEAN == 0 )) && echo "Expected a video-name, aborting" 1>&2 && exit 1
[ "$VIDEO" != "" ] && (( $DO_CLEAN == 1 )) && echo "Must either print video information, or pass --clean, but not both, aborting" 1>&2 && exit 1
(( $DO_CLEAN == 1 )) && echo "Removing cache directory: $CACHE_D" && rm -rf "$CACHE_D" && exit 0

# -- execute
CACHE_F="$CACHE_D/$VIDEO.video-length"
[ -e "$CACHE_F" ] && (( $RECACHE == 1 )) && rm -rf "$CACHE_F"

# Do we need to create the cache file?
if [ "$VIDEO" != "" ] && ! [ -e "$CACHE_F" ] ; then
   SRCF="/net/$(darpa-video-info.sh $VIDEO --key SERVER)/aux/qobi/video-datasets/$(darpa-video-info.sh $VIDEO --key MAIN_CORPUS)/$VIDEO/video-length"
   mkdir -p "$(dirname "$CACHE_F")"
   [ -s "$SRCF" ] && cat "$SRCF" > "$CACHE_F" 
   ! [ -s "$SRCF" ] && echo "Failed to find non-empty video-length file: $SRCF, aborting" && exit 1
fi

[ "$VIDEO" != "" ] && cat "$CACHE_F"
   
exit 0


