Isabelle/MMT
============

This tool requires requires a development snapshot after Isabelle2018 (from
https://isabelle.sketis.net/devel/release_snapshot). It needs to be initialized
as Isabelle component, e.g. in ```$ISABELLE_HOME_USER/etc/settings``` like this:

    init_component ".../MMT/src/mmt-isabelle"

* build (requires sbt): ```isabelle mmt_build```

* importer: e.g. ```isabelle mmt_import -v ZF```

* HTTP server: ```isabelle mmt_server```

* MMT shell: ```isabelle mmt```

* development: ```isabelle env idea```

  Note that ```build.sbt``` has some tricks on conditional project composition:
  it requires re-initializing the IDEA project after dropping in or out of
  Isabelle system context.
