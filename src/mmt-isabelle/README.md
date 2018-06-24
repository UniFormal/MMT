Isabelle/MMT
============

This tool requires a formal Isabelle system context (with certain environment
variables, additions to CLASSPATH, and access to the source distribition).

Before the Isabelle2018 release it requires a recent development snapshot
from https://isabelle.sketis.net/devel/release_snapshot

  * Build: ```isabelle env sbt mmt/deploy```
  * Run: ```isabelle java info.kwarc.mmt.api.frontend.Run```

It is also possible to build / run everything within IntelliJ IDEA, after starting it via ```isabelle env idea```. Note that ```build.sbt``` has some tricks on conditional project composition: it requires re-initializing the IDEA project after dropping in or out of Isabelle system context.