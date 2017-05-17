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

# Compile and install emacs (not for mac, in case of mac use brew system)
case `uname` in
    Darwin)
        brew cask install emacs # FIXME: add packages !
        ;;
    *)
        # Install needed packages
        (
            rm -rf emacs-25.2.tar.gz emacs-25.2
            wget https://ftp.gnu.org/gnu/emacs/emacs-25.2.tar.gz;
            tar xzvf emacs-25.2.tar.gz;
            cd emacs-25.2;
            ./autogen.sh;
            ./configure --prefix=$PREFIX --with-x-toolkit=gtk3; # --with-xwidgets --without-pop --with-mailutils;
            make bootstrap -j $NB_PROC;
            make install
        )
        ;;
esac
