package info.kwarc.mmt

import api._
import documents._
import modules._
import symbols._
import objects._
import frontend._
import libraries._
import archives._

/** This is the main package of the MMT API.
  *
  * It holds subpackages for all data structures, data containers, and the central algorithms and services.
  *
  * =Classes directly defined in the package=
  * 
  * Some minor classes that are used throughout MMT are defined in this package, in particular:
  *  - MMT URIs in the class [[Path]] and [[Namespace]]
  *  - processing and content errors in the class [[Error]]
  *  
  *  The package also contains root classes for certain types that are subclassed throughout the package.
  *  Most importantly:
  *  - [[StructuralElement]]: structure-level (= named) parts in the data structures for the MMT language: documents, theories, constants, ... 
  *  - [[MMTTask]]: tasks for a single object: parsing, checking, ...
  *  - [[Rule]]: object-level part of the MMT language that is written in Scala 
  *
  * =Subpackages= 
  * 
  * ==Data structures for the MMT language==
  * 
  * The data structures for the MMT languages are defined in 4 packages corresponding to the 4 levels:
  * - [[documents]]: [[Document]]s and all other [[NarrativeElement]]s
  * - [[modules]]: [[Module]]s (= the toplevel declarations), in particular [[Theory]]s and [[View]]s
  * - [[symbols]]: all [[Declaration]]s inside modules, in particular [[Constant]]s 
  * - [[objects]]: all anonymous [[Obj]]ects (e.g., formulas, functions, etc.), in particular [[Context]]s and [[Term]]s
  * 
  * The former 3 levels are jointly called 'structural' levels.
  * All elements subclass [[StructuralElement]], have an MMT URI, and carry an MMT URI referring to their parent in the MMT abstract syntax tree.
  *
  * Structural elements are extensible (via [[DerivedModule]]s and [[DerivedDeclaration]]s),
  * and the package [[patterns]] defines declaration patterns as a built-in extension. 
  * 
  * All structural elements are mutable and implement the [[ContainerElement]] interface for changing their children.
  * Objects, by contrast, are represented as immutable inductive types.(except for carrying [[metadata.Metadata]] and [[objects.ClientProperties]]).
  * The boundary between structural elements and objects is mediated by [[ComponentContainer]]s:
  * these are mutable, owned by structural elements, and maintain objects.
  *
  * A few auxiliary data structures shifted to separate packages:
  * - [[opaque]]: external (i.e., informal, computation) content
  * - [[informal]]: partially outdated informal data structures 
  * - [[metadata]]: metadata annotations to all structural elements or objects
  *
  * ==The MMT main class and its internal state==
  *  
  * The package [[frontend]] contains the class [[Controller]], which owns all state relevant for running MMT.
  * Typically, each application creates a single instance of this class.
  * The package also defines several other essential classes, most importantly MMT's extension (=plug-in, add-on) interfaces via the [[Extension]] class.
  *
  * The package [[libraries]] maintains the instances of MMT language data structures, in particular the [[Library]] class.
  * [[Controller]] owns a [[Library]], which stores all structural elements that have been loaded into memory.
  *
  * ==User interfaces==
  * 
  * The package [[frontend]] also contains the main executable classes, in particular the [[Shell]] class.
  * 
  * The package [[gui]] collects all classes for building graphical user interfaces.
  * This includes auxiliary classes for use in IDE plugins.
  * 
  * The package [[web]] collects all classes for the HTTP interface of MMT.
  *
  * ==Physical storage of the MMT language files==
  * 
  * The package [[archives]] defines MMT [[Archive]]s (= projects) as well as classes for building and working with archives.
  * The latter include in particular the [[BuildManager]] and [[BuildTarget]].
  * Build targets include [[Importer]]s and [Exporter]]s that translate between MMT and other formats. 
  * 
  * The package [[backend]] defines classes for maintaining archives and
  * translating between the MMT URIs of structural elements and their physical storage locations.
  * 
  * ==The central algorithms for processing MMT content==
  * 
  * The processing model of MMT consists of several major algorithms. 
  * - [[parser]]: read strings into MMT data structures
  * - [[checking]]: check and refine MMT data structures
  * - [[uom]]: pure computation on MMT data structures
  * - [[proving]]: theorem proving on MMT data structures (in very early state))
  * - [[execution]]: imperative computation (in very, very early state)
  * - [[presentation]]: rendering MMT data structures in user-facing formats (including HTML+presentation MathML)
  *
  * All algorithms are defined in [[Extension]]s coupled with default implementations.
  * Moreover, all algorithms are split into two separate levels, one for structural elements and objects. See [[LeveledExtension]].
  *
  * The package [[notations]] maintains the common code for parsing and presentation.
  *
  * The package [[valuebases]] maintains mathematical databases as a part of MMT. 
  * 
  * ==Other algorithms on the MMT data structures==
  * 
  * The package [[ontology]] contains a relational, semantic web-style ontology and query engine for it.
  *
  * The package [[moc]] contains change management.
  * 
  * The package [[refactoring]] contains refactoring principles.
  * 
  * ==General purpose utility functions==
  * 
  * The package [[utils]] defines general purpose APIs for files, URIs, HTML building, etc.
  *
  */
package object api {
}
