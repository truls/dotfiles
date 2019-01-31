#!/bin/bash

if [ $# -lt 1 ]; then
	echo "Usage: setpdftitle.sh \"DESTINATION DIRECTORY\""
	exit 1
fi

dest=$1
temp=`mktemp`

while read f; do
	if [ ! -d $dest ]; then
		mkdir -p $dest 2>/dev/null
		if [ $? -gt 0 ]; then
		  echo "Could not create directory $dest"
		  exit 1
		fi
	fi
	title=`basename -s.pdf "$f"`
	pdftk "$f" dump_data_utf8 output $temp
	sed -ie "/.*Title$/!b;n;cInfoValue: $title" $temp
	pdftk "$f" update_info_utf8 $temp output $dest/"$f"
done

rm $temp

