# Setting up a development environment

1. Set up MMT

   As described at [MMT's installation page](https://uniformal.github.io//doc/setup/), set up a development environment for MMT with IntelliJ IDEA (under "Option 1b" at time of writing).
   You do not need to run `java -jar mmt.jar` or anything like that (as described under "Step 3").

2. [Install UFrameIT/archives](https://github.com/UFrameIT/archives).

3. Open the IntelliJ created in step 1 and locate `src -> frameit-mmt -> src -> info.kwarc.mmt.frameit.communication.Server` in the project browser and run it via the green triangle:

   ![Project browser showing `info.kwarc.mmt.frameit.communication.Server`](https://i.imgur.com/J75FzWa.png)

   This will invoke compilation and execution of the server in that order. Compilation hopefully works. See below when you get a stack overflow error *at compilation*. Execution is supposed to result in an error since the server expects some command-line arguments upon execution. We will add them next.

4. Edit the `Server` run configuration

   - first open all run configurations:

     ![run configurations](https://i.imgur.com/nFd8ETr.png)

   - edit the `Server` configuration by adding `-bind :8085 -archive-root <path to archives root from step 2>` to its program arguments:

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
   WARNING: Illegal reflective access by com.twitter.jvm.Hotspot (.../com/twitter/util-jvm_2.12/20.7.0/util-jvm_2.12-20.7.0.jar) to field sun.management.ManagementFactoryHelper.jvm
   WARNING: Please consider reporting this to the maintainers of com.twitter.jvm.Hotspot
   WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations
   WARNING: All illegal access operations will be denied in a future release
   ```

   Don't worry. The first warning is ignorable &mdash; log output is just discarded. The second one is [Twitter's to fix and is under their investigation already](https://github.com/twitter/util/issues/266) (for months, sadly).

**You're done.**

The server should now expose [its REST API](./README.md#rest-api) at `http://localhost:8085`.
As a first test, open <http://localhost:8085/debug/space/print> with your browser. After a few seconds (on cold start), it should output something like

```mmt
theory DefaultSituationSpace : http://mathhub.info/FrameIT/frameworld?FrameworldMeta  = 
  theory DefaultSituationSpace/Root : http://mathhub.info/FrameIT/frameworld?FrameworldMeta  = 
    include http://mathhub.info/FrameIT/frameworld?OppositeLen ❙
    include http://mathhub.info/FrameIT/frameworld?AngleSum ❙
    include http://mathhub.info/FrameIT/frameworld?Midpoint ❙
  ❚
❚
```

## Stack overflow error when compiling

The Scala compiler of older IntelliJ versions sometimes (unreproducibly) runs into stackoverflow errors when compiling, concretely, when typechecking. Try updating to the latest IntelliJ version. Apart from that, the Internet does not offer many tips for solving this except increasing the stack size for compilation:

- <https://github.com/scala-js/scala-js/issues/3588>
- <https://github.com/scala/bug/issues/9696>

Not sure if it helped in my case or the error just randomly disappeared.

## Building and Usage of `frameit.jar`

Instead of running the server from within IntelliJ, we can also build a self-contained `frameit.jar` (~70 MiB) comprising of the server, all MMT dependencies, and Scala library dependencies.

For development, we recommend always running the server from within IntelliJ IDEA.
Building `frameit.jar` is however necessary for deploying a [UFrameIT release](https://github.com/UFrameIT/UFrameIT/releases).

**Building:**

1. Open your shell and navigate to `<mmt repo clone>/src`.
2. Run `sbt`
3. Run `frameit/deploy` within `sbt`.

   Upon success, this will produce the file `<mmt repo clone>/deploy/frameit.jar`.

**Usage:** `java -jar frameit.jar ...` where `...` are the *required* command-line options from step 4 from the beginning of this file.