Isabelle/MMT
============

## Requirements

Until the next official release after Isabelle2018 (presumably Isabelle2019
in June 2019), Isabelle/MMT requires a suitable development snapshot from
https://isabelle.sketis.net/devel/release_snapshot or a repository clone
from http://isabelle.in.tum.de/repos/isabelle -- see also its
`README_REPOSITORY` file with the all-important **Quick start in 30min**.

In particular, the following versions from Nov-2018 should fit together:

  * Isabelle/1bee990d443c from https://isabelle.in.tum.de/repos/isabelle
  * AFP/1566e3fc3118 from https://bitbucket.org/isa-afp/afp-devel
  * MMT/55f297239a45 from https://github.com/UniFormal/MMT/commits/devel
  * MathHub/MMT/urtheories/efe7963422a3 from
    https://gl.mathhub.info/MMT/urtheories/commits/devel

The corresponding OMDoc content is available here:

  * https://gl.mathhub.info/Isabelle/Distribution (version 3faa7f9c7742)
  * https://gl.mathhub.info/Isabelle/AFP (version 30a0dc372eaa)


## Setup

Isabelle/MMT is a command-line tool within the Isabelle system environment.
Both building and running it requires proper Isabelle component setup, which
results in certain environment variables in the enclosing process. For
general explanations, see the Isabelle **system** manual (chapter 1), e.g.
available in the *Documentation* panel of Isabelle/jEdit. The MMT
sub-directory `src/mmt-isabelle` already provides suitable component
settings that can be activated e.g. in `$ISABELLE_HOME_USER/etc/settings`
like this:

    init_component ".../MMT/src/mmt-isabelle"

where `.../MMT` refers to the full path specification of the MMT source
directory. Note that a standalone `mmt.jar` is not sufficient: the Isabelle
component requires some regular files from the MMT source tree.


## Examples

Here are some example invocations of the main command-line tools:

* build (requires `sbt`):

      isabelle mmt_build

* importer:

      isabelle mmt_import -B ZF
      isabelle mmt_import HOL
      isabelle mmt_import -B HOL-Analysis
      isabelle mmt_import -o record_proofs=2 -B HOL-Proofs

* HTTP server to browse the results:

      isabelle mmt_server -A isabelle_test

* MMT shell:

      isabelle mmt

* development with IntelliJ IDEA:

      isabelle env idea

  Note that ```build.sbt``` has some tricks on conditional project
  composition: it requires to re-initialize the IDEA project after dropping
  in or out of the Isabelle system environment.


## Big examples

Recall that Isabelle consists of two processes:

  1. ML: `poly` for the Poly/ML runtime system
  2. Scala: `java` for the Java Runtime Environment

Big examples require generous heap space for both, typically 30 GB. Note
that both platforms have a discontinuity when switching from short 32-bit
pointers to full 64-bit ones: *4 GB* for Poly/ML and *32 GB* for Java. Going
beyond that doubles the baseline memory requirements.

The subsequent setup works well for hardware with 64 GB of main memory:

  * `$ISABELLE_HOME_USER/etc/preferences`

        ML_system_64 = true

  * `$ISABELLE_HOME_USER/etc/settings`

        ML_OPTIONS="--minheap 4G --maxheap 30G"

        ISABELLE_TOOL_JAVA_OPTIONS="-Djava.awt.headless=true -Xss16m -Xms4g -Xmx30g"

Examples:

      isabelle mmt_import -a -X doc -X no_doc
      isabelle mmt_import -d '$AFP' -B HOL-Analysis -X doc -X no_doc -X slow
      isabelle mmt_import -d '$AFP' -a -X doc -X no_doc -X slow
      isabelle mmt_import -d '$AFP' -a -X doc -X no_doc -X very_slow

Here `$AFP` refers to the Isabelle settings variable provided by the Archive
of Formal Proofs as Isabelle component (using a suitable `init_component`
line in `$ISABELLE_HOME_USER/etc/settings`). Alternatively, it is possible
to refer to the physical AFP directory as `.../AFP/thys` (the sub-directory
`thys` is important here).


## Directory layout for MathHub

The MMT MathHub keeps the main Isabelle Distribution separate from the
Archive of Formal Proofs. The `isabelle mmt_import` tool can use the chapter
structure of Isabelle sessions to distribute documents into different
archives. Its option `-C` provides (multiple) mappings from chapter names to
archive names (according to the base directory name). The chapter name `_`
serves as catch-all pattern.

Since all sessions in AFP are guaranteed to belong to the chapter `AFP`, the
following works for Isabelle + AFP as one big import process:

      isabelle mmt_import -d '$AFP' -A content/MathHub -C AFP=AFP -C _=Distribution -a -X doc -X no_doc -X slow -x HOL-ODE-Numerics -x Diophantine_Eqns_Lin_Hom -x HLDE

Note that other Isabelle applications may have their own chapter naming
scheme, or re-use official Isabelle chapter names; if nothing is specified,
the default chapter is `Unsorted`.


Command-line reference
======================

The entry points for Isabelle/MMT tools reside in the MMT directory
`src/mmt-isabelle/lib/Tools` -- usually some shell scripts that invoke Java
within the Isabelle environment, to pass the control over to some Scala
`main()` functions. The actual implementation is provided by regular Scala
functions, without assuming a command-line context.

The usage of command-line tools generally follows GNU bash standards: the
Isabelle/Scala library provides an imitation of GNU `getopts` for that.
Output and error behavior follows established standards for Isabelle
command-line tools. In particular, internal Java exception traces are *not*
shown to end-users by default.


## isabelle mmt_build

This is a thin wrapper for `sbt mmt/deploy` within the formal Isabelle
environment and the correct directory in the MMT source tree. It also
ensures that Isabelle/Scala has been properly bootstrapped beforehand (e.g.
when working from the Isabelle repository).


## isabelle mmt_import

This is the main Isabelle/MMT importer: it explores Isabelle sessions and
loads all resulting theories into an Isabelle/PIDE process. Results are
continuously imported into MMT and written to specified math archive
directories. Its command-line usage is as follows:

    Usage: isabelle mmt_import [OPTIONS] [SESSIONS ...]

      Options are:
        -A DIR       add archive directory
        -C CH=AR     add mapping of chapter CH to archive AR, or default "_=AR"
        -B NAME      include session NAME and all descendants
        -D DIR       include session directory and select its sessions
        -R           operate on requirements of selected sessions
        -X NAME      exclude sessions from group NAME and all descendants
        -a           select all sessions
        -d DIR       include session directory
        -g NAME      select session group NAME
        -l NAME      logic session name (default: "Pure")
        -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
        -v           verbose mode
        -x NAME      exclude session NAME and all descendants

      Import specified sessions into MMT archive directories.

Options `-A` and `-C` specify target archives and a mapping from Isabelle
session chapters to MMT archives. Chapter names are provided in Isabelle
session `ROOT` specifications, and archive names are the base directory
names: if it is the same as the chapter name, the directory will be used at
its root; otherwise the chapter becomes a sub-directory of the archive. The
directories given via (multiple) `-A` options are recursively searched for
MMT archives. If the result is empty, a fresh archive is initialized
according to Isabelle system options `mmt_archive_dir` etc. Thus it is
possible to use `isabelle mmt` without any archive options and get results
into a default directory (`isabelle_test`), freshly initialized on demand.

Options `-B`, `-D`, `-R`, `-X`, `-a`, `-d`, `-g`, `-x` with remaining
non-option arguments provide the standard Isabelle vocabulary to specify
sessions, e.g. see `isabelle build` described in the **system** manual (with
examples).

Option `-o` allows to augment the environment of Isabelle system options,
before invoking the main import process; see again the **system** manual. A
typical example is `-o threads=8` to specify the number of ML threads, or
`-o skip_proofs` to skip actual proof checking. Note that Isabelle/MMT also
provides its own options in `src/mmt-isabelle/etc/options` (with short
descriptions).

Option `-v` enables verbose mode, similar to `isabelle build`.

Option `-l` allows to use a different base logic images than the `Pure`. It
rarely makes sense to use something else, because formal content between
`Pure` and the base logic is *missing* from the import.


## isabelle mmt_server

This is a thin wrapper for the regular MMT web server. It refers to archives
in a similar manner as `isabelle mmt_import`; thus it is useful to explore
the results of that tool. The command-line usage is as follows:

    Usage: isabelle mmt_server [OPTIONS]

      Options are:
        -A DIR       add archive directory
        -p PORT      server port (default: 8080)

      Start MMT HTTP server on localhost, using specified archive directories.

Option `-A` refers to archive directories that are recursively explored as
in `isabelle mmt_build`. In order to refer to the default archive of that
tool (option `-I` above), it needs to be included explicitly as `-A` here:
e.g. `isabelle mmt_server -A isabelle_test`.

Option `-p` specifies an alternative HTTP server port.


## isabelle mmt

This is a thin wrapper to the standard MMT shell, running within the
Isabelle system environment, with both the `mmt.jar` and the Isabelle/Scala
jars in the Java name space. In particular, dropping into its `scala`
toplevel allows to explore Isabelle and MMT functionality interactively on
the Scala toplevel.

The command-line usage is the same as for the regular MMT shell.