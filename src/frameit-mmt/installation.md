## Installation

1. Get a set of UFrameIT archives you want the server to use: `git clone --recursive https://github.com/UFrameIT/archives archive-root`

   Remember the path you clone this to!

2. Clone the MMT repository on devel branch: `git clone --branch devel https://github.com/UniFormal/mmt`

3. Import the source code into a new IntelliJ project: see <https://uniformal.github.io//doc/setup/devel#using-intellij>

4. Open the just created IntelliJ project and locate `src -> frameit-mmt -> src -> info.kwarc.mmt.frameit.communication.Server` in the project browser and run it via the green triangle:

   ![Project browser showing `info.kwarc.mmt.frameit.communication.Server`](https://i.imgur.com/J75FzWa.png)

   This will invoke compilation and execution of the server in that order. Compilation hopefully works. See below when you get a stack overflow error *at compilation*. Execution is supposed to result in an error since the server expects some command-line arguments upon execution. We will add them next.

5. Edit the `Server` run configuration

   - first open all run configurations:

     ![run configurations](https://i.imgur.com/nFd8ETr.png)

   - edit the `Server` configuration by adding `-bind :8085 -archive-root <path to archive root>` to its program arguments:

     ![program arguments](https://i.imgur.com/lZahL6C.png)

   - for debugging, add the `-debug` there, too. Upon server start, instead of an empty situation theory, this will use a pre-filled situation theory within the `FrameIT/frameworld` archive.

6. Rerun the server via the run configuration dropdown (left to green triangle in IntelliJ's menu band)

   The server should now be running. The initial console output should be

   ```
   "C:\Program Files (x86)\OpenJDK\jdk-14.0.1\bin\java.exe" [...] info.kwarc.mmt.frameit.communication.Server

   SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
   SLF4J: Defaulting to no-operation (NOP) logger implementation
   SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.

   WARNING: An illegal reflective access operation has occurred
   WARNING: Illegal reflective access by com.twitter.jvm.Hotspot (file:/C:/Users/nroux/Desktop/mmt/src/null/Coursier/cache/v1/https/repo1.maven.org/maven2/com/twitter/util-jvm_2.12/20.7.0/util-jvm_2.12-20.7.0.jar) to field sun.management.ManagementFactoryHelper.jvm
   WARNING: Please consider reporting this to the maintainers of com.twitter.jvm.Hotspot
   WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations
   WARNING: All illegal access operations will be denied in a future release
   ```

   Don't worry. The first warning is ignorable &mdash; log output is just discarded. The secone one is Twitter's to fix and is under their investigation already (for months, sadly).

**You're done.** The server should now be available at `http://localhost:8085` and respond to the REST API calls detailled below.

As a first test, you can try opening <http://localhost:8085/debug/space/print>. It should output something like

```
//TODO: actually it now prints more :)
"\ntheory SituationTheory : http://mathhub.info/FrameIT/frameworld?FactCollection  = \n❚"
```

## Stack overflow error when compiling

The Scala compiler sometimes (unreproducibly) runs into stackoverflow errors when compiling, concretely, when typechecking. Try updating to the latest IntelliJ version. Apart from that, the Internet does not offer many tips for solving this except increasing the stack size for compilation:

- <https://github.com/scala-js/scala-js/issues/3588>
- <https://github.com/scala/bug/issues/9696>

Not sure if it helped in my case or the error just randomly disappeared.
