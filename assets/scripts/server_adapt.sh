#!/bin/bash

# Dealing with options
while getopts ":j:h" opt; do
    case $opt in
        j)
            NB_PROC=$OPTARG
            echo "parallel mode activated with $NB_PROC process" >&2
            ;;
        h)
            echo "$0 [-j <nb_proc>] <prefix>" >&2
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            ;;
    esac
done
shift $((OPTIND-1))

if [ $# -lt 1 ]
then
    echo "$0 [-j <nb_proc>] <prefix>" >&2
    exit -1
fi

PREFIX=$1


# Add a link to the binary (but first move the previous binary to be sure we are not having pb.)
if [ ! -e $PREFIX/bin/emacs-bin ]
then
    mv $PREFIX/bin/emacs $PREFIX/bin/emacs-bin
    wget https://raw.githubusercontent.com/seblemaguer/emacs-reproducible-research/master/assets/scripts/emacs-bin.sh -O $PREFIX/bin/emacs
fi
