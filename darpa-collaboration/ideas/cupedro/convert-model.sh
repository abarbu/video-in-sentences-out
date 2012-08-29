#!/bin/bash

EXEC="$(basename "$0")"
DETECTORDIR="$HOME/darpa-collaboration/pedro/detector"
IROBOTDIR="$HOME/darpa-collaboration/pkg/irobot_libcudafelz_1.1"
TMPD="$(mktemp -d "/tmp/$EXEC-XXXXXXXXXX")"
STARDIR="$TMPD/detector/star-cascade"
TMPF="$TMPD/convert.m"

# --- be a good citizen
trap clean_up EXIT
clean_up()
{
    rm -rf "$TMPD"
    stty sane
}

# --- the absolute pathname of a file
abs_path()
{
    FILENAME="$1"
    echo "$(cd "$(dirname "$FILENAME")"; pwd)/$(basename "$FILENAME")"
}

# --- remove a file extension
extensionless()
{
    FILENAME="$1"
    EXT="$(echo "$FILENAME" | awk -F . '{if (NF>1) { print $NF }}')"
    EXTENSIONLESS="$FILENAME"
    (( ${#EXT} > 0 )) && EXTENSIONLESS="${FILENAME:0:$(expr ${#FILENAME} -  ${#EXT} - 1)}"
    echo "$EXTENSIONLESS"
}


# --- help message
show_help()
{
    cat <<EOF

   Usage: $EXEC [[--pca] csc_model.mat]+

      Converts 1 or more cascade models from matlab format to
      irobot's cuda format. The new model files are written to
      the present-working directory, with the extension
      '.irobot-felz-model'.

   Options:

      --pca Exports a pca file (csv file) for the next specified model

   Example:

      # Creates 4 files in the present-working-directory
      $EXEC bicycle.mat closet.mat person.mat rake.mat

      # Creates a pca file for person.mat, and exports
      # models for person.mat and closet.mat
      $EXEC --pca person.mat person.mat closet.mat

   Depends:

      irobot distribution unpacked at:
         $IROBOTDIR

      A working copy of Felzenszwalb at:
         $DETECTORDIR

EOF
}

# ---------------------------------------------------------------------------- Parse arguments
(( $# == 0 )) && echo "Expected at least 1 argument -- type -h for help" 1>&2 && exit 1
(( $# > 0 )) && [ "$1" = "-h" ] || [ "$1" = "--help" ] && show_help && exit 0

# ---------------------------------------------------------------------- Check matlab is alive
! [ -d "$DETECTORDIR" ] && echo "Failed to find felzenszwalb directory: $DETECTORDIR, aborting." 1>&2 && exit 1
! [ -d "$IROBOTDIR" ] && echo "Failed to find irobot directory: $IROBOTDIR aborting." 1>&2 && exit 1

# ------------------------------------------------------------------------ Build matlab script
echo "% -- Run from the detector directory" >> "$TMPF"
echo "addpath('$STARDIR');" >> "$TMPF"
echo "addpath('$STARDIR/..');" >> "$TMPF"
echo >> "$TMPF"
while (( $# > 0 )) ; do
   FILE="$1"
   PCA=0
   shift

   if [ "$FILE" = "--pca" ] ; then
	 (( $# == 0 )) && echo "Expected a filename after --pca, skipping." 1>&2 && continue
	 FILE="$1"
	 PCA=1
	 shift
   fi

   ! [ -e "$FILE" ] && echo "File not found: $FILE, skipping" 1>&2 && continue
   ABSFILE="$(abs_path "$FILE")"

   if (( $PCA == 0 )) ; then
	 OUTFILE="$(pwd)/$(basename "$(extensionless "$ABSFILE")").irobot-felz-model"

	 echo "% -- Export $FILE to $(basename "$OUTFILE")" >> "$TMPF"
	 echo "load('$ABSFILE');" >> "$TMPF"
	 echo "csc_model.export_file = '$OUTFILE';" >> "$TMPF"
	 echo "pyra = featpyramid(ones(16,16,3), csc_model);" >> "$TMPF"
	 echo "[dCSC, bCSC] = cascade_detect(pyra, csc_model, csc_model.thresh);" >> "$TMPF"
	 echo >> "$TMPF"

   else
	 OUTFILE="$(pwd)/$(basename "$(extensionless "$ABSFILE")")_pca_coeff.csv"
	 echo "% -- Export pca from $FILE to $(basename "$OUTFILE")" >> "$TMPF"
	 echo "load('$ABSFILE');" >> "$TMPF"
	 echo "csvwrite('$OUTFILE', csc_model.coeff);" >> "$TMPF"
	 echo >> "$TMPF"
   fi

done

echo "% -- Exit matlab" >> $TMPF
echo "exit" >> $TMPF
echo "" >> $TMPF

# --------------------------------------------------- ---------- Build irobots export facility
MODEL_CC=model.cc
MODEL_H=model.h
CUDA_FELZ=cudafelz_cascade_model.m
IROBOT_SSD=$IROBOTDIR/matlab/voc-release4/star-cascade

! [ -e "$IROBOT_SSD/$MODEL_CC" ] && echo "Failed to file file $IROBOT_SSD/$MODEL_CC, aborting." && exit 1

cp -r $DETECTORDIR $TMPD/

echo "Build matlab felz with irobot's model.cc file"
cp $IROBOT_SSD/$MODEL_CC $STARDIR/
cp $IROBOT_SSD/$MODEL_H $STARDIR/
[ -e $IROBOT_SSD/$CUDA_FELZ ] && cp $IROBOT_SSD/$CUDA_FELZ $STARDIR/
cp $IROBOTDIR/matlab/voc-release4/run_converter.m $STARDIR/..
cd $STARDIR
! make && echo "Failed to build irobot's model exporting facility, aborting..." && exit 1

# ------------------------------------------------------------------------  Execute
echo "Executing matlab script:"
echo
cat "$TMPF" | sed 's/^/   /'
echo

cd $TMPD
matlab -nodesktop -nosplash -nojvm -r "convert" >/dev/null

# Matlab does something to the terminal, and this resets it
stty sane
