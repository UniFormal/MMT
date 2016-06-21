package info.kwarc.mmt

import api._

/** This is the main package of the MMT API.
  *
  * It holds subpackages for all data structures, data containers, and the central algorithms and
  * services.
  *
  * Some classes that are used everywhere are defined in this package, in particular:
  *  - MMT URIs in the class [[Path]]
  *  - processing and content [[Error]]s
  *
  * The subpackages are

  *  - The MMT data structures are defined in the packages
  *   - [[documents]]
  *   - [[modules]]
  *   - [[symbols]]
  *   - [[objects]]
  *    The former 3 statefully maintain their child content and use an MMT URI to refer to their parent element.
  *    Objects are stateless (except for metadata and [[objects.ClientProperties]]).
  *
  *  - The package [[frontend]] maintains several classes that are essential for understanding the whole code.
  *
  *   Data structures and containers for associated content are defined in the packages
  *   - [[opaque]]: external (i.e., informal, computation) content
  *   - [[ontology]]: relational data, query language, and query engine
  *   - [[metadata]]: metadata annotations (stateful children of content)
  *
  *   - The central data container is in the package
  *   - [[libraries]]: maintains the diagram of theories
  *
  *   - Central algrorithms are defined in the packages
  *   - [[parser]]: reading of native MMT text syntax
  *   - [[presentation]]: HTML+presentation MathML generation
  *   - [[notations]]: the common code or parsing and presentation
  *   - [[checking]]: structural checking, type reconstruction
  *   - [[proving]]: theorem prover
  *   - [[uom]]: simplification of objects
  *   - [[moc]]: change management
  *
  *   All algorithms are defined in two separate levels for structure and objects.
  *   See [[frontend.LeveledExtension]].
  *
  *   - User and machine interfaces are defined in the packages
  *    - [[gui]]: graphical
  *    - [[web]]: HTTP
  *    - [[frontend]]: shell and scala-toplevel prompt
  *
  *   - Interfaces to physical storage are defined in the packages
  *    - [[backend]]: abstraction from physical storage
  *    - [[archives]]: MMT archives (= projects), build system, also a kind of physical storage
  *
  *   The package defines general purpose APIs for files, URIs, HTML building, etc.
  *
  */
package object api {
}
