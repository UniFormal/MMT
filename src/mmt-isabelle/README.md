Isabelle/MMT
============

This tool requires requires a development snapshot after Isabelle2018 (from
https://isabelle.sketis.net/devel/release_snapshot). It needs to be initialized
as Isabelle component, e.g. in ```$ISABELLE_HOME_USER/etc/settings``` like this:

    init_component ".../MMT/src/mmt-isabelle"

* build (requires sbt):

      isabelle mmt_build

* importer: e.g.

      isabelle mmt_import ZF
      isabelle mmt_import -B ZF
      isabelle mmt_import HOL
      isabelle mmt_import -B FOL
      isabelle mmt_import -B HOL-Analysis
      isabelle mmt_import -o record_proofs=2 -B HOL-Proofs

* HTTP server:

      isabelle mmt_server

* MMT shell:

      isabelle mmt

* development:

      isabelle env idea

  Note that ```build.sbt``` has some tricks on conditional project composition:
  it requires re-initializing the IDEA project after dropping in or out of
  Isabelle system context.


Big examples
------------

This requires 64-bit setup Poly/ML and generous Java stack/heap space, e.g. as follows:

  * `$ISABELLE_HOME_USER/etc/preferences`

        ML_system_64 = true

  * `$ISABELLE_HOME_USER/etc/settings`

        ML_OPTIONS="--minheap 4G --maxheap 30G"

        ISABELLE_TOOL_JAVA_OPTIONS="-Djava.awt.headless=true -Xss16m -Xms4g -Xmx30g"

Examples:

      isabelle mmt_import -a -X doc
      isabelle mmt_import -d '$AFP' -B HOL-Analysis -X slow
      isabelle mmt_import -d '$AFP' -B HOL-Analysis
