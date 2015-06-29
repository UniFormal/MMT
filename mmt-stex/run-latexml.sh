#!/bin/bash

# require exactly one argument
if [ "$#" -ne 1 ]
then
  echo "usage: $(basename $0) <file>[.tex]"
  exit 1
fi

inputfile="$(readlink -f $1)"

theory="$(basename $inputfile .tex)"

# ignore all.*tex files
if [ "$theory" == "all" -o "${theory:0:4}" == "all." ]
then
  echo "ignoring argument: $1"
  exit 0
fi

dir="$(dirname $inputfile)"

sourceDir="$dir"
source="$(basename $sourceDir)"
# nonsense happens without "source"
while [ ! "$source" == "source" -a ! "$source" == "/" ]
do
  sourceDir="$(dirname $sourceDir)"
  source="$(basename $sourceDir)"
done

if [ ! "$source" == "source" ]
then
  echo "missing \"source\" path segment in input: $inputfile"
  exit 0
fi

repoDir="$(dirname $sourceDir)" # strip off final "source" path segment

repo="$(basename $repoDir)"
groupDir="$(dirname $repoDir)"
group="$(basename $groupDir)"
baseDir="$(dirname $groupDir)"
mathHubDir="$(dirname $baseDir)"
localMHDir="$(dirname $mathHubDir)"
moreStyles="${localMHDir}/sty"

# take directory for LaTeXML, LaTeXMLs, sTeX and perl5lib
# from mathHubDir or external EXT_BASE
: ${EXT_BASE:=${mathHubDir}/ext}

export LATEXML_BASE=${EXT_BASE}/LaTeXML
export STEXSTYDIR=${EXT_BASE}/sTeX/sty
export TEXINPUTS=.//:${moreStyles}:${STEXSTYDIR}//:
export PERL5LIB=${EXT_BASE}/perl5lib/lib/perl5:${LATEXML_BASE}/blib/lib

# if localpaths.tex exists, we take it as it is
if [ ! -f "$dir/localpaths.tex" ]
then
echo "creating $dir/localpaths.tex"
cat << EOF > "$dir/localpaths.tex"
% this file defines root path local repository
\defpath{MathHub}{$baseDir}
\mhcurrentrepos{$group/$repo}
\input{$repoDir/lib/WApersons}
% we also set the base URI for the LaTeXML transformation
\baseURI[\MathHub{}]{https://mathhub.info/$group/$repo}
EOF
fi

lang="${theory##*.}"

if [ "$lang" == "$theory" ] # has no extension
then
  lang=""
else
  dotlang=".$lang"
fi

preamble1="$repoDir/lib/pre.tex"
postamble1="$repoDir/lib/post.tex"
preamble2="$repoDir/lib/pre$dotlang.tex"
postamble2="$repoDir/lib/post$dotlang.tex"

if [ -f "$preamble2" ]
then
  preamble="$preamble2"
else
  preamble="$preamble1"
  if [ ! "$lang" == "" ]
  then
    echo "missing preamble for language: $lang"
  fi
fi

if [ -f "$postamble2" ]
then
  postamble="$postamble2"
else
  postamble="$postamble1"
  if [ ! "$lang" == "" ]
  then
    echo "missing postamble for language: $lang"
  fi
fi

if [ ! -f "$preamble" -o ! -f "$postamble" ]
then
  echo "missing pre- or postamble files: $preamble, $postamble"
  exit 0
fi

cd $sourceDir  # source directory

scriptname="$(basename $0 .sh)"

if [ "$scriptname" == "run-latexml" ]
then
 export PATH=${EXT_BASE}/perl5lib/bin:$PATH # for latexmls

 exec ${EXT_BASE}/perl5lib/bin/latexmlc --quiet --profile stex-smglom-module \
  --path="${moreStyles}" "$dir/${theory}.tex" \
  --destination="$dir/${theory}.omdoc" \
  --log="$dir/${theory}.ltxlog" \
  --preamble="$preamble" \
  --postamble="$postamble" \
  --expire=10
else
 cat "$preamble" "$dir/${theory}.tex" "$postamble" \
  | pdflatex -jobname ${theory} -interaction scrollmode
fi
