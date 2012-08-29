#!/bin/bash

####################################### variables
EXECNAME="$(basename "$0")"
METRIC_IU="*intersection-union-metric*"
METRIC_IU_SQRT="*sqrt-intersection-union-metric*"
METRIC_CENTRE="*centre-distance-metric*"
METRIC_DIAG_OLAP="*diagonal-overlap*"
OUTDIR=/aux/$USER/tmp/boxx/log
CACHEDIR=/aux/$USER/tmp/boxx/cache
SOURCEF="$HOME/darpa-collaboration/ideas/inter-track-reliability.sc"
DARPA_BUILD_DIR="$HOME/darpa-collaboration/ideas/$(architecture-path)"
DSCI="$DARPA_BUILD_DIR/dsci"
TMPF="$(mktemp /tmp/.$EXECNAME.XXXXX)"
declare -a PIDS

####################################### show-help
show_help()
{
    cat <<EOF

Usage: $EXECNAME N METRIC 

   N      - The number of simultaneous processes to run.
   METRIC - The distance metric to use. Can be one of either:
               centre: the distance between box centres
               iu: box-area-intersection divided by box-area-union
               sqrt-iu: the square-root of iu
               diag-olap: overlap of diagnol lines on other box

   Calculating inter-rater reliabilities is a time-consuming 
   operation, so results are cached in CACHEDIR, which is
   $CACHEDIR. If you stop a collection of processes, and then start 
   the jobs over again, video-team-scores will not be recalcuated. 
   (You can always delete the cache, or change the passed argument 
   if you wish.)

   The calculation processes are monitored by this script. Exiting
   (or killing) the script will kill any executing processes.

Notes:

   + Uses source file $SOURCEF 
   + Log files are written to $OUTDIR
   + CACHEDIR is $CACHEDIR

Examples:

   # Using 48 simultaneous processes, calculate the distance 
   # metric for box-area-intersection divided by box-area-union.
   ./$EXECNAME 48 iu

   # Calculate the distance metric for centre-distances.
   ./$EXECNAME 48 centre

EOF

}

####################################### kill-tree-ex
killtree_ex() {
    local _pid="$1"
    local _sig=${2-TERM}
    for _child in $(ps -o pid --no-headers --ppid ${_pid}); do
        killtree_ex "${_child}" ${_sig}
    done
    kill -${_sig} "${_pid}"
}

####################################### trap EXIT
trap clean_up EXIT
clean_up()
{
    rm -rf $TMPF
    for P in ${PIDS[@]} ; do
	kill -0 $P 2>/dev/null && echo -n "Killing $P - " && killtree_ex $P 2>/dev/null 1>/dev/null && echo "done."
    done
}

####################################### process the ith part of the list
process_i()
{
    I=$1
    N=$2
    cat $HOME/darpa-collaboration/ideas/inter-track-reliability.sc | awk "{ print \$0 } END { print \"(set! *box-coder-data-dir* \\\"$CACHEDIR\\\")\n(process-part-of-list $N $I $METRIC)\" }" | darpa-wrap "$DSCI" 2>&1
}

####################################### parse cmd-args
(( $# == 1 )) && [ "$1" = "-h" ] || [ "$1" = "--help" ] && show_help && exit 0
(( $# != 2 )) && echo "Error: expected 2 arguments; displaying usage." 1>&2 && show_help && exit 1

N="$1"
[ "$N" -ne "$N" 2>/dev/null ] && echo "Error: expected a positive integer for N" 1>&2 && exit 1
[ "$N" -lt "1" ] && echo "Error: expected a positive integer for N" 1>&2 && exit 1

METRIC=
[ "$2" == "iu" ] && METRIC="$METRIC_IU"
[ "$2" == "sqrt-iu" ] && METRIC="$METRIC_IU_SQRT"
[ "$2" == "centre" ] && METRIC="$METRIC_CENTRE"
[ "$2" == "diag-olap" ] && METRIC="$METRIC_DIAG_OLAP"
[ "$METRIC" = "" ] && echo "Error: expected either 'iu' or 'centre' for METRIC" 1>&2 && exit 1

####################################### check environment
mkdir -p $OUTDIR
! [ -d "$OUTDIR" ] && echo "Failed to create log directory: $OUTDIR, aborting." 1>&2 && exit 1
! [ -e "$SOURCEF" ] && echo "Failed to find source file $SOURCEF, aborting." 1>&2 && exit 1
[ "" = "$(which darpa-wrap)" ] && echo "Failed to find darpa-wrap on path, aborting." 1>&2 && exit 1
! [ -e "$DSCI" ] && echo "Failed to find dsci at $DSCI, aborting." 1>&2 && exit 1


####################################### execute simultaneous processes
I=0
while (( $I < $N )) ; do
    process_i $I $N > $OUTDIR/$I.log &
    PIDS[$I]=$!
    I=$(expr $I + 1)
done

####################################### monitor the processes
DOLOOP=1
while (( $DOLOOP == 1 )) ; do
    NA=0
    I=0

    echo > $TMPF
    for P in ${PIDS[@]} ; do

	if ! kill -0 $P 2>/dev/null ; then
	    NA=$(expr $NA + 1)
        else
	    echo "+ Process $I (pid=$P) - EXECUTING." >> $TMPF
	fi

        I=$(expr $I + 1)
	if (( $I == $N )) ; then
	    clear
	    date "+%Y-%m-%d %T"
	    echo "$NA of $N processes complete"
	    echo "Logs written to $OUTDIR"
	    echo "Output cached to $CACHEDIR"
            echo "Metric is $METRIC"
	    cat $TMPF
	fi

	# Exit if NA ever gets to N
	(( $NA == $N )) && DOLOOP=0
    done
    (( $DOLOOP == 1 )) && sleep 4
done

####################################### finish -- display results
echo
echo "Sub-processes complete, calculating results..."
echo
process_i 0 1 | awk 'BEGIN { found=0; } { if( $2 == "Results" ) found=1; if( found == 1 ) print $0; if( found == 1 && substr($1,0,1) == "#" && $2 != "Results" ) found = 0; }'
echo

