# frameit-mmt: Server component of FrameIT project

This is the server component of the [FrameIT project](https://kwarc.info/systems/frameit/), maintained by [@ComFreek](https://github.com/ComFreek).

## Installation

1. Get a set of UFrameIT archives you want the server to use: `git clone --recursive https://github.com/UFrameIT/archives archive-root`

   Remember the path you clone this to!

2. Clone the MMT repository on devel branch: `git clone --branch devel https://github.com/UniFormal/mmt`

3. Import the source code into a new IntelliJ project: see <https://uniformal.github.io//doc/setup/devel#using-intellij>

4. Open the just created IntelliJ project and locate `src -> frameit-mmt -> src -> info.kwarc.mmt.frameit.communication.Server` in the project browser and run it via the green triangle: 

   ![Project browser showing `info.kwarc.mmt.frameit.communication.Server`](https://i.imgur.com/J75FzWa.png)
  
   This will probably result in an error &mdash; don't worry, we are just missing some command-line arguments that we will in next.

5. Edit the `Server` run configuration

   - first open all run configurations:
  
     ![run configurations](https://i.imgur.com/nFd8ETr.png)

   - edit the `Server` configuration by adding `-bind :8085 -archive-root <path to archive root>` to its program arguments:
  
     ![program arguments](https://i.imgur.com/lZahL6C.png)

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

As a first test, you can try opening <http://localhost:8085/debug/situationtheory/print>. It should output something like

```
"\ntheory SituationTheory : http://mathhub.info/FrameIT/frameworld?FactCollection  = \n❚"
```

## REST API

```
POST /archive/build-light
  no payload
POST /archive/build
  no payload

POST /fact/add
  payload: {"label": "some label", "tp": OMDoc JSON term, "df": OMDoc JSON term or null or left out}
  return: {"uri": uri to created fact}
GET /fact/list
  no payload
  return: [
    {"uri": uri to fact, "label": "some label", "tp": {"original": OMDoc JSON term, "simplified": OMDoc JSON term}, df: same as tp or null or left out},
    // repeat for other facts
  ]

GET /scroll/list
  [{
      "problemTheory": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem",
      "solutionTheory": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Solution",
      "label": "OppositeLen",
      "description": "Given a triangle ABC right angled at C, the distance AB can be computed from the angle at B and the distance BC",
      "requiredFacts": [{
        "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pA",
        "label": "pA",
        "tp": {
          "original": OMDoc JSON term,
          "simplified": OMDoc JSON term
        },
        "df": same as tp or null or left out
      } /* more facts */]
    } /* more scrolls]

POST /scroll/apply
  {
    "scroll": {
      "problemTheory": "uri to problem theory",
      "solutionTheory": "uri to solution theory",
    },
    "assignments": [
      ["uri to fact", OMDoc JSON term]
    ]
  }

GET debug/situationtheory/print
  no payload
  return: string of MMT surface syntax (not to be parsed; debugging only!)
```

## Sample output for `scroll/list`

```
[
    {
        "problemTheory": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem",
        "solutionTheory": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Solution",
        "label": "OppositeLen",
        "description": "Given a triangle ABC right angled at C, the distance AB can be computed from the angle at B and the distance BC",
        "requiredFacts": [
            {
                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pA",
                "label": "pA",
                "tp": {
                    "original": {
                        "uri": "http://mathhub.info/MitM/core/geometry?3DGeometry?point",
                        "kind": "OMS"
                    },
                    "simplified": {
                        "uri": "http://mathhub.info/MitM/core/geometry?3DGeometry?point",
                        "kind": "OMS"
                    }
                },
                "df": null
            },
            {
                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pB",
                "label": "pB",
                "tp": {
                    "original": {
                        "uri": "http://mathhub.info/MitM/core/geometry?3DGeometry?point",
                        "kind": "OMS"
                    },
                    "simplified": {
                        "uri": "http://mathhub.info/MitM/core/geometry?3DGeometry?point",
                        "kind": "OMS"
                    }
                },
                "df": null
            },
            {
                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pC",
                "label": "pC",
                "tp": {
                    "original": {
                        "uri": "http://mathhub.info/MitM/core/geometry?3DGeometry?point",
                        "kind": "OMS"
                    },
                    "simplified": {
                        "uri": "http://mathhub.info/MitM/core/geometry?3DGeometry?point",
                        "kind": "OMS"
                    }
                },
                "df": null
            },
            {
                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pdistBC_v",
                "label": "pdistBC_v",
                "tp": {
                    "original": {
                        "uri": "http://mathhub.info/MitM/Foundation?RealLiterals?real_lit",
                        "kind": "OMS"
                    },
                    "simplified": {
                        "uri": "http://mathhub.info/MitM/Foundation?RealLiterals?real_lit",
                        "kind": "OMS"
                    }
                },
                "df": null
            },
            {
                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pdistBC",
                "label": "pdistBC",
                "tp": {
                    "original": {
                        "applicant": {
                            "uri": "http://mathhub.info/FrameIT/frameworld?DistanceFact?distanceFact",
                            "kind": "OMS"
                        },
                        "arguments": [
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pB",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pC",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pdistBC_v",
                                "kind": "OMS"
                            }
                        ],
                        "kind": "OMA"
                    },
                    "simplified": {
                        "applicant": {
                            "uri": "http://mathhub.info/FrameIT/frameworld?DistanceFact?distanceFact",
                            "kind": "OMS"
                        },
                        "arguments": [
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pB",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pC",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pdistBC_v",
                                "kind": "OMS"
                            }
                        ],
                        "kind": "OMA"
                    }
                },
                "df": null
            },
            {
                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pangleABC_v",
                "label": "pangleABC_v",
                "tp": {
                    "original": {
                        "uri": "http://mathhub.info/MitM/Foundation?RealLiterals?real_lit",
                        "kind": "OMS"
                    },
                    "simplified": {
                        "uri": "http://mathhub.info/MitM/Foundation?RealLiterals?real_lit",
                        "kind": "OMS"
                    }
                },
                "df": null
            },
            {
                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pangleABC",
                "label": "pangleABC",
                "tp": {
                    "original": {
                        "applicant": {
                            "uri": "http://mathhub.info/FrameIT/frameworld?AngleFact?angleFact",
                            "kind": "OMS"
                        },
                        "arguments": [
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pA",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pB",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pC",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pangleABC_v",
                                "kind": "OMS"
                            }
                        ],
                        "kind": "OMA"
                    },
                    "simplified": {
                        "applicant": {
                            "uri": "http://mathhub.info/FrameIT/frameworld?AngleFact?angleFact",
                            "kind": "OMS"
                        },
                        "arguments": [
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pA",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pB",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pC",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pangleABC_v",
                                "kind": "OMS"
                            }
                        ],
                        "kind": "OMA"
                    }
                },
                "df": null
            },
            {
                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pangleBCA",
                "label": "pangleBCA",
                "tp": {
                    "original": {
                        "applicant": {
                            "uri": "http://mathhub.info/FrameIT/frameworld?AngleFact?angleFact",
                            "kind": "OMS"
                        },
                        "arguments": [
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pB",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pC",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pA",
                                "kind": "OMS"
                            },
                            {
                                "float": 90.0,
                                "kind": "OMF"
                            }
                        ],
                        "kind": "OMA"
                    },
                    "simplified": {
                        "applicant": {
                            "uri": "http://mathhub.info/FrameIT/frameworld?AngleFact?angleFact",
                            "kind": "OMS"
                        },
                        "arguments": [
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pB",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pC",
                                "kind": "OMS"
                            },
                            {
                                "uri": "http://mathhub.info/FrameIT/frameworld?OppositeLen_Problem?pA",
                                "kind": "OMS"
                            },
                            {
                                "float": 90.0,
                                "kind": "OMF"
                            }
                        ],
                        "kind": "OMA"
                    }
                },
                "df": null
            }
        ]
    }
]
``` 
