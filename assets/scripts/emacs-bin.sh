#!/bin/zsh

EMACS_DIR=$HOME/environment/local/bin
EMACS_CMD=$EMACS_DIR/emacs-bin
EMACS_CLIENT_CMD=$EMACS_DIR/emacsclient

CURTMPDIR=/tmp
if [ "$TMPDIR" != "" ]
then
    CURTMPDIR=$TMPDIR
fi

cmd=`echo "$*" | grep -c -- "-\(stop\)"`
if [ $cmd -ne 0 ]
then
    USERID=`id | sed 's/uid=\([0-9][0-9]*\)(.*/\1/'`
    if test -e $CURTMPDIR/emacs$USERID/server
    then
        $EMACS_CLIENT_CMD -e "(kill-emacs)"
        rm -rf $CURTMPDIR/emacs$USERID/server
    else
        echo "emacs already stopped"
    fi
    exit 0;
fi

# Check command line
cmd=`echo "$*" | grep -c -- "-\(batch\|version\|debug-init\|Q\|q\)"`
if [ $cmd -ne 0 ]
then
    #echo $cmd
    cmd=($EMACS_CMD $*)
    #echo "...$cmd..."
    $cmd
    exit $?
else


	cmd=`echo "$*" | grep -c -- "-\(nw\|t\|tty\)"`
	# Emacs client specific
	if [ $cmd -ne 0 ]
	then
		$EMACS_CLIENT_CMD $*
	# Normal!
	else
		USERID=`id | sed 's/uid=\([0-9][0-9]*\)(.*/\1/'`
		if test -e $CURTMPDIR/emacs$USERID/server
		then
			echo "Ready."
		else
			echo "Starting server."
			$EMACS_CMD --daemon
		fi
    	$EMACS_CLIENT_CMD -c "$@"
	fi
fi
