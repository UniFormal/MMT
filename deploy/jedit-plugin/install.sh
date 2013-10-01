dir=`dirname $0`
jedit=$1

# copy the jars
cp $dir/plugin/jars/*.jar $jedit/jars

# copy the modes
cp $dir/plugin/modes/*.xml $jedit/modes

# remove the final </MODES> tag in the mode catalog, append the entries for the modes, and then append the final </MODES> tag again
sed -i 's/<\/MODES>//' $jedit/modes/catalog
cat $dir/plugin/modes/catalog >> $jedit/modes/catalog
echo \</MODES\> >> $jedit/modes/catalog

# append abbreviations for the mmt mode
cat $dir/plugin/abbrevs >> $jedit/abbrevs