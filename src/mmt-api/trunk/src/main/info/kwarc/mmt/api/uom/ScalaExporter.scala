package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import archives._
import modules._
import symbols._
import objects._
import notations._
import documents._

object GenericScalaExporter {
   /** reserved identifiers */
   private val keywords = List("true", "false", "type", "val", "var", "def", "class", "trait", "object", "extends", "with", "while", "do", "for")
   /** preused identifiers, i.e., declared in Object */
   private val reserved = List("eq", "List", "Set", "String")
   /** escapes strings to avoid clashes with Scala keywords */
   private def escape(s:String) : String = {
      if (keywords.contains(s))
        "`" + s + "`"
      else if (reserved.contains(s))
        "N" + s
      else escapeChars(s)
   }
   private def escapeChars(s: String) =
      // TODO: temporary hack, check characters are legal in Scala IDs
      s flatMap {c => if (c == '≃') "" else c.toString}

   def nameToScalaQ(p: GlobalName) = (p.module.toMPath.name.toPath + "_" + escapeChars(p.name.toPath)).replace(".", "__")
   def nameToScala(l: LocalName) = escape(l.toPath.replace("/","."))
   /** package URI */
   def dpathToScala(d: DPath, key: List[String] = Nil) = {
      val u = d.uri
      var auth = utils.stringToList(u.authority.getOrElse(""), "\\.").reverse
      (auth ::: key ::: u.path).mkString(".")
   }
   def scalaToDPath(j: String, key: List[String] = Nil) = {
      val segments = j.split("\\.").toList
      val i = segments.indexOfSlice(key)
      val (auth,path) = if (key == Nil || i == -1)
         (segments.reverse, Nil)
      else
         (segments.take(i).reverse, segments.drop(i+key.length))
      DPath(utils.URI("http", auth.mkString(".")) / path)
   }
   /** package URI . modname */
   def mpathToScala(m: MPath, key: List[String] = Nil) = dpathToScala(m.doc, key) + "." + nameToScala(m.name)
   val imports = "import info.kwarc.mmt.api._\n" + "import objects._\n" + "import uom._\n" +
    "import ConstantScala._\n"

   def scalaVal(name: String, tp: String): String = "  val " + name + " : " + tp
   def scalaVal(name: GlobalName, tp: String): String = scalaVal(nameToScalaQ(name), tp)

   def scalaValDef(name: String, tp: Option[String], df: String): String = {
      val tpS = tp.map(": " + _).getOrElse("")
      "  lazy val " + name + tpS + " = " + df
   }
   def scalaValDef(name: GlobalName, tp: Option[String], df: String): String = scalaValDef(nameToScalaQ(name), tp, df)

   def scalaDef(name: String, args: List[(String,String)], ret: String): String = {
      val argsB = if (args.isEmpty) "" else args.map({case (x,y) => s"$x: $y"}).mkString("(", ", ", ")")
      "  def " + name + argsB + ": " + ret
   }
   def scalaDef(name: GlobalName, args: List[(String,String)], ret: String): String = scalaDef(nameToScalaQ(name), args, ret)

   def scalaType(name: GlobalName): String = "  type " + nameToScalaQ(name)

}

import GenericScalaExporter._

/** This trait bundles auxiliary methods for exporting Scala code */
trait GenericScalaExporter extends Exporter {
   override val outExt = "scala"
   override protected val folderName = "NAMESPACE"
   val packageSep: List[String] = Nil

   protected def outputHeader(dp: DPath) {
      val pack = dpathToScala(dp, packageSep)
      rh.writeln("package " + pack)
      rh.writeln(imports)
   }
   /**
    * generates a trait with abstract members for each constants
    * @param typeToScala yields the input and output type for a constant
    */
   protected def outputTrait(t: DeclaredTheory)(doCon: Constant => String) {
     val includes = t.getIncludesWithoutMeta.filter {i =>
        controller.globalLookup.getO(i) match {
           // we only take basic theories for now
           case Some(d: DeclaredTheory) => d.name.length == 1
           case _ => false
        }
     }
     val includesSF = includes.filter {i =>
       controller.globalLookup.getO(i) match {
         case Some(r: RealizationInScala) => false //TODO handle includes of models
         case Some(t: DeclaredTheory) => true
         case _ => false // should not happen
       }
     }
     val includesS = includesSF.map(i => " with " + mpathToScala(i, packageSep)).mkString("")
     val tpathS = t.path.toString
     val name = nameToScala(t.name)
     rh.writeln(s"/** The type of realizations of the theory $tpathS */")
     rh.writeln(s"trait $name extends RealizationInScala$includesS {")
     val domainOver = if (includes == "") "" else "override " // override included values
     rh.writeln(s"  ${domainOver}val _domain: TheoryScala = $name\n")
     t.getPrimitiveDeclarations foreach {
        case c: Constant =>
             val d = doCon(c)
             rh.writeln(d)
        //TODO exclude declarations with extraneous types that should not be implemented, e.g., m:MOR a b
        case SimpleStructure(s, fromPath) if ! s.isInclude =>
             // unnamed structures have been handled above already
             rh.writeln("  val " + nameToScalaQ(s.path) + ": " + mpathToScala(fromPath))
        case _ =>
     }
     rh.writeln("}\n")
   }

   /**  generates a companion object fieds for the MMT URIs
    *   @param extraFields fields appended to the object
    */
   protected def outputCompanionObject(t: DeclaredTheory)(extraFields: Constant => String) {
     val tpathS = t.path.toString
     val name = nameToScala(t.name)
     rh.writeln(s"/** Convenience functions for the MMT URIs of the declarations in the theory $tpathS\n" +
                 "    along with apply/unapply methods for them */")
     rh.writeln(s"object $name extends TheoryScala {")
     val baseUri = t.parent.uri
     rh.writeln("  val _base = DPath(utils.URI(\"" + baseUri.scheme.getOrElse("") +
        "\", \""+ baseUri.authority.getOrElse("") +"\")" +
        baseUri.path.foldRight("")((a,b) => " / \""+ a + "\"" + b) +
        ")"
     )
     rh.writeln("  val _name = LocalName(\"" + t.name + "\")")
     t.getPrimitiveDeclarations foreach {
        case c: Constant =>
            var o = ""
            o +=  s"\n  object ${nameToScala(c.name)} extends ConstantScala {\n"
            o +=  s"    val parent = _path\n"
            o +=   "    val name = \"" + c.name + "\"\n"
            o += extraFields(c)
            o += "  }"
            rh.writeln(o)
         case _ =>
      }
     rh.writeln("\n}")
   }

   /** produces code to instantiate [[uom.DocumentScala]] to iterate over all content */
   def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
      var pack = dpathToScala(dpath, packageSep)
      if (pack == "") pack = "content" // dpath is empty URI for the content folder
      rh.writeln("//Source file generated by MMT\n")
      rh.writeln(s"package $pack")
      rh.writeln("import info.kwarc.mmt.api.uom._\n\n")
      rh.writeln(s"object $folderName extends DocumentScala {")
      modules foreach {case bt =>
         controller.globalLookup.getModule(bt.contentMPath) match {
            case m: DeclaredView =>
               val p = s"$pack.${nameToScala(m.name)}"
               rh.writeln(s"  addRealization($p)")
            case _ =>
         }
      }
      namespaces foreach {case bt =>
         val p = dpathToScala(bt.contentDPath, packageSep)
         rh.writeln(s"  addDocument($p.$folderName)")
      }
      rh.writeln("}")
   }
   /** do nothing by default */
   def exportDocument(doc : Document, bt : BuildTask) {}
}

class ScalaExporter extends GenericScalaExporter {
   val outDim = Dim("export", "scala")
   val key = "scala"

   private def arityToScala(arity: Arity) : List[(String,String)] = arity.components.map {
      case Arg(n,_) => ("x" + n.abs, "Term")
      case ImplicitArg(n,_) => ("x" + n.abs, "Term")
      case SeqArg(n,_,_) => ("xs" + n.abs, "List[Term]")
      case Var(n,_,None,_) => ("v" + n, "VarDecl")
      case Var(n,_,Some(_),_) => ("vs" + n, "Context")
   }

   private def lastArgIsSeq(arity: Arity) = ! arity.arguments.isEmpty && arity.arguments.last.isSequence
   private def lastVarIsSeq(arity: Arity) = ! arity.variables.isEmpty && arity.variables.last.isSequence

   private def applyMethods(arity: Arity) : String = {
     val scalaArgs = arityToScala(arity)
     // x1 :: ... :: xn :: Nil or x1 :: ... :: xsn
     var argListString   = scalaArgs.map(_._1).mkString(" :: ")
     if (! lastArgIsSeq(arity))
        argListString = argListString + ":: scala.Nil"
     // (x1, ..., xn) or x1
     var argTupleString   = scalaArgs.map(_._1).mkString(", ")
     if (scalaArgs.length > 1)
        argTupleString = s"($argTupleString)"
     // (T1, ..., Tn) or T1
     var tpString    = scalaArgs.map(_._2).mkString(", ")
     if (scalaArgs.length > 1)
        tpString = s"($tpString)"
     // x1: T1, ..., xn: Tn
     val argtpString = scalaArgs.map(p => p._1 + ": " + p._2).mkString(", ")
     // def apply(...) : Term = ...
     val app = if (arity.isApplication)
         s"    def apply($argtpString) = OMA(OMID(this.path), $argListString)\n"
     else if (arity.isPlainBinder)
         s"    def apply(vs1: Context, s2: Term) = OMBIND(OMID(this.path), vs1, s2)\n"
     else if (arity.isConstant)
        ""
     else
         "  // no apply method generated for this arity\n"
     // def unapply(t: Term): Option[...] = ...
     val unapp = if (arity.isApplication)
         s"    def unapply(t: Term): Option[$tpString] = t match {\n" +
         s"      case OMA(OMID(this.path), $argListString) => Some($argTupleString)\n" +
         s"      case _ => None\n" +
         s"    }\n"
     else if (arity.isPlainBinder)
         s"    def unapply(t: Term): Option[(Context, Term)] = t match {\n" +
         s"      case OMBIND(OMID(this.path), vs1, s2) => Some((vs1, s2))\n" +
         s"      case _ => None\n" +
         s"    }\n"
     else if (arity.isConstant)
        ""
     else
         "  // no unapply methods generated for this arity\n"

     app + unapp
   }

   def exportTheory(t: DeclaredTheory, bf: BuildTask) {
      outputHeader(t.parent.doc)
      //var constants : List[String] = Nil
      outputTrait(t) {c =>
         val arity = c.not.map(_.arity).getOrElse(Arity.constant)
         val scalaArgs = arityToScala(arity)
         //constants ::= nameToScalaQ(c.path)
         scalaDef(c.path, scalaArgs, "Term")
      }
      outputCompanionObject(t) {c =>
         c.not.map(n => applyMethods(n.arity)).getOrElse("")
      }
      //constants.reverse.mkString("  declares(", ",", ")\n"))
   }

   def exportView(v: DeclaredView, bf: BuildTask) {}
}
