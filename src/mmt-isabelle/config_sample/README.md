Example configuration
=====================

Hardware
========

* E.g. a cheap "cloud" node: 8 CPU cores, 64 GB RAM

* Note that Java 8 does not work well with many cores: it is difficult to
restrain it to a healthy amount of virtual "processors" (8..16).


OS packages
===========

E.g. standard Ubuntu or Debian. Isabelle is self-contained: it provides its
own Java, Scala etc. (also used for MMT).

Some optional packages enable further examples (which are disabled by
default):

  * ghc
  * mlton
  * ocaml
  * smlnj
  * swi-prolog

See also various environment variables in `etc/settings` below.


Isabelle configuration
======================

See etc/settings and etc/preferences -- these should be moved to
$ISABELLE_HOME_USER (cf. `isabelle getenv ISABELLE_HOME_USER`).


Example command-line invocation
===============================

    # build Isabelle/MMT tool
    isabelle mmt_build

    # import Isabelle (without documentation sessions)
    isabelle mmt_import -A content/MathHub -C _=Distribution -a -X doc -X no_doc

    # import Isabelle + AFP (without exceedingly slow sessions)
    isabelle mmt_import -d '$AFP' -A content/MathHub -C AFP=AFP -C _=Distribution -a -X doc -X no_doc -X slow -x HOL-ODE-Numerics -x Diophantine_Eqns_Lin_Hom -x HLDE

This takes about 1 day. It puts AFP sessions into the "Isabelle/AFP"
archive, and everything else into the "Isabelle/Distribution" archive of
MathHub (https://gl.mathhub.info/Isabelle).
