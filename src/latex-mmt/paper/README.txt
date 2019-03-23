This folder contains the system description of MMTTeX.

Running "pdflatex paper.tex" produces paper.tex.mmt and paper.pdf
Running "mmttex paper.tex.mmt" produces papaer.text.mmt.sty
Running "pdflatex paper.tex" now produces the desired paper.pdf

This is dependent on the following one-time actions:
- having an MMT server run on port 8080
  needed because mmttex run MMT via that port
- running once "mmt build ARCHIVE mmt-sty" on the archives MMT/urtheories and MMT/examples
  needed to build the sty files for the background knowledge
- setting mmttexmathhhubroot in the preamble of paper.tex to MMT's content folder on the local system
  needed for pdflatex to find the sty files generated in the previous step
