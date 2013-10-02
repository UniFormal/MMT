dir=`dirname $0`/plugin
jedit=$1

# copy the jars
cp $dir/jars/*.jar $jedit/jars

# copy mars and startup file to MMT plugin directory
mkdir $jedit/plugins/info.kwarc.mmt.jedit.MMTPlugin
cp $dir/plugins/info.kwarc.mmt.jedit.MMTPlugin/* $jedit/plugins/info.kwarc.mmt.jedit.MMTPlugin/ 

# copy the modes
cp $dir/plugin/modes/*.xml $jedit/modes

# remove the final </MODES> tag in the mode catalog
sed -i 's/<\/MODES>//' $jedit/modes/catalog
# remove old entries for MMT modes (if any)
sed -i '/<!--MMT:-->/d' $jedit/modes/catalog
# append the entries for the MMT modes
cat $dir/plugin/modes/catalog >> $jedit/modes/catalog
# append the final </MODES> tag again
echo \</MODES\> >> $jedit/modes/catalog

# remove old abbreviation for the mmt mode (if any)
sed -i '/\[mmt]/???' $jedit/abbrevs
# append abbreviations for the mmt mode
cat $dir/plugin/abbrevs >> $jedit/abbrevs