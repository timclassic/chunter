text=""
while read line
do
        if [ "EOF" == "$line" ]; then
                echo "$text" | vmadm $@
                exit $?
        else
		if [ "xx" != "x$textx" ]; then
			text="$text\n"
		fi
                text="$text$line"
        fi
done
