package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import archives._
import documents._
import modules._
import notations._
import symbols._

object GenericScalaExporter {
  /** reserved identifiers */
  private val keywords = List("type", "val", "var", "def", "return", "new", "class", "trait", "object", "extends", "with", "abstract", "implicit",
      "match", "case", "if", "else", "while", "do", "for", "throw", "try", "catch", "finally")
  /** preused identifiers, i.e., declared in Object */
  private val reserved = List("true", "false", "eq", "List", "Nil", "Set", "String", "Boolean", "Option", "None", "Some", "Term", "OML", "Context", "VarDecl")

  /** escapes strings to avoid clashes with Scala keywords */
  private def escape(s: String): String = {
    if (keywords.contains(s))
      "`" + s + "`"
    else if (reserved.contains(s))
      "_" + s
    else escapeChars(s)
  }

  // TODO: do this cleanly
  private def escapeChars(s: String) =
    s.replace("_","_underscore_").replace("≃","_cong_").replace("-","_minus_")
  private def unescapeChars(s: String) =
    s.replace("_cong_","≃").replace("_minus_","-").replace("_underscore_", "_")


  def nameToScalaQ(p: GlobalName) = (p.module.name.toPath + "_" + escapeChars(p.name.toPath)).replace(".", "__")

  def nameInScala(p: GlobalName) = (p.module.name.toPath).replace(".", "__") + "." + nameToScala(p.name) + ".path"

  def nameToScala(l: LocalName) = escape(l.toPath.replace("/", "."))

  /** package URI */
  def dpathToScala(d: DPath, key: List[String] = Nil) = {
    val u = d.uri
    var auth = utils.stringToList(u.authority.getOrElse(""), "\\.").reverse
    (auth ::: key ::: u.path).map(escapeChars).mkString(".")
  }

  def scalaToDPath(j: String, key: List[String] = Nil) = {
    val segments = j.split("\\.").toList.map(unescapeChars)
    val i = segments.indexOfSlice(key)
    val (auth, path) = if (key == Nil || i == -1)
      (segments.reverse, Nil)
    else
      (segments.take(i).reverse, segments.drop(i + key.length))
    val authO = if (auth.isEmpty) None else Some(auth.mkString("."))
    DPath(utils.URI(Some("http"), authO) / path)
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

  def scalaDef(name: String, args: List[(String, String)], ret: String): String = {
    val argsB = if (args.isEmpty) "" else args.map({ case (x, y) => s"$x: $y" }).mkString("(", ", ", ")")
    "  def " + name + argsB + ": " + ret
  }

  def scalaDef(name: GlobalName, args: List[(String, String)], ret: String): String = scalaDef(nameToScalaQ(name), args, ret)

  def scalaType(name: GlobalName): String = "  type " + nameToScalaQ(name)

  /** name and type of a Scala variable declarations */
  case class Argument(name: String, tp: String, sequence: Boolean) {
    def decl = name + ": " + tp
  }
  case class ArgumentList(args: List[Argument]) {
    def +(that: ArgumentList) = ArgumentList(this.args ::: that.args)
    // if at most the last argument is a sequence, this appends ::Nil if needed
    def finalNil = if (args.lastOption.exists(a => !a.sequence)) " :: Nil" else ""
    // x1, ..., xn
    def names = args.map(_.name)
    // T1, ..., Tn
    def types = args.map(_.tp)
    // x1: T1, ..., xn: Tn
    val decls = args.map(_.decl)
    // x1: T1, ..., xn: Tn
    val declsConsed = decls.map(d => "(" + d + ")").mkString(" :: ") + finalNil
    // List(x1,...,xn)
    def nameList = args.map {a => if (a.sequence) a.name else "List(" + a.name + ")"}.mkString(":::")
    // x1::x2:: ... :: xn [:: Nil]
    def namesConsed = names.mkString(" :: ") + finalNil
    // (x1, ..., xn) or x1
    def tuple = if (args.length == 1) names.head else names.mkString("(", ", ", ")")
    def tupleType = if (args.length == 0) "Unit" else if (args.length == 1) types.head else types.mkString("(", ", ", ")")
  }
  /** produces Scala source for apply/unapply methods
   *  @param first the variables
   *  @param second the arguments
   */
  abstract class Operator(first: ArgumentList, second: ArgumentList) {
    def combined = first+second
    def tuple = combined.tuple
    def tupleType = combined.tupleType
    def context = combined.decls.mkString(",")
    /** maps a list of variables and a list of arguments to a term */
    def makeTerm(pattern: Boolean): String
    // def apply(x1,...,xn) : Term = term
    def applyMethod = List(s"def apply($context): Term = ${makeTerm(false)}")
    def unapplyMethod = List(
       s"def unapply(t: Term): Option[$tupleType] = t match {",
       s"  case ${makeTerm(true)} => Some($tuple)",
       s"  case _ => None",
       s"}"
    )
    def methods = applyMethod ::: unapplyMethod
  }

  /** for MMT terms using OMS, OMA, and OMBIND */
  class MMTOperator(p: ContentPath, f: ArgumentList, s: ArgumentList) extends Operator(f,s) {
    private def omid = "OMID(this.path)"
    def makeTerm(pattern: Boolean): String = {
      val sS = if (pattern) s.declsConsed else s.nameList
      if (f.args.isEmpty && s.args.isEmpty)
        omid
      else if (f.args.isEmpty)
        s"OMA($omid, $sS)"
      else
        s"OMBINDC($omid, ${f.nameList}, $sS)"
    }
  }
}

/** This trait bundles auxiliary methods for exporting Scala code */
class GenericScalaExporter extends Exporter {
  import GenericScalaExporter._

  val key = "mmt-scala"
  override val outExt = "scala"
  override protected val folderName = "NAMESPACE"
  val packageSep: List[String] = Nil

  /* top level export methods */

  def exportTheory(t: Theory, bf: BuildTask): Unit = {
    rh.writeln(s"// Auto-generated file for theory ${t.path}")

    outputHeader(t.parent.doc)
    outputTrait(t)
    outputCompanionObject(t)
  }

  def exportView(v: View, bf: BuildTask): Unit = {}

  /** produces code to instantiate [[uom.DocumentScala]] to iterate over all content */
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]): Unit = {
    var pack = dpathToScala(dpath, packageSep)
    if (pack == "") pack = "content" // dpath is empty URI for the content folder
    rh.writeln("//Source file generated by MMT\n")
    rh.writeln(s"package $pack")
    rh.writeln("import info.kwarc.mmt.api.uom._\n\n")
    rh.writeln(s"object $folderName extends DocumentScala {")
    modules foreach {bt =>
      val m = controller.globalLookup.getModule(bt.contentMPath)
      rh.writeln(outputModuleEntry(m))
    }
    namespaces foreach { case bt =>
      val p = dpathToScala(bt.contentDPath, packageSep)
      rh.writeln(s"  addDocument($p.$folderName)")
    }
    rh.writeln("}")
  }

  /** command for a module to be added to the namespace object, override as needed */
  def outputModuleEntry(m: Module) : String = ""

  /** do nothing by default */
  def exportDocument(doc: Document, bt: BuildTask): Unit = {}

  /* theories are exported as 3 parts: header, optional trait, object */

  protected def outputHeader(dp: DPath): Unit = {
    val pack = dpathToScala(dp, packageSep)
    rh.writeln("package " + pack)
    rh.writeln(imports)
  }

  /**
   * generates the trait, empty by default, override as needed
   */
  protected def outputTrait(t: Theory): Unit = {}

  /* the companion object */

  /** generates a companion object with fields for the MMT URIs
    * @param t the theory
    */
  protected def outputCompanionObject(t: Theory): Unit = {
    val tpathS = t.path.toString
    val name = nameToScala(t.name)
    rh.writeln(s"/** Convenience functions for the MMT URIs of the declarations in the theory $tpathS\n" +
      "    along with apply/unapply methods for them */")
    rh.writeln(s"object $name extends TheoryScala {")
    val baseUri = t.parent.uri
    rh.writeln(s"  val _base = DPath(utils.URI(${toParsableString(baseUri.scheme)}, ${toParsableString(baseUri.authority)}, abs=true)" +
      baseUri.path.map(s => " / " + toParsableString(s)).mkString + ")"
    )
    rh.writeln("  val _name = LocalName(\"" + t.name + "\")")
    t.getPrimitiveDeclarations foreach {
      case c: Constant =>
        val extraFields = companionObjectFields(c)
        // TODO handle structures
        if (c.name.length == 1) {
          val lines = List(
            s"",
            s"object ${nameToScala(c.name)} extends ConstantScala {",
            s"  val parent: MPath = _path\n",
             "  val name: String = \"" + c.name + "\""
          ) ::: extraFields.map("  " + _) ::: List(
            s"}"
          )
          lines foreach {l => rh.writeln("  " + l)}
        }
      case _ =>
    }
    rh.writeln("\n}")
  }

  private def toParsableString(v: Any) : String = v match {
    case Nil => "Nil"
    case List(a @_*) => (a map toParsableString).mkString("List(", ", ", ")")
    case Some(a) => s"Some(${toParsableString(a)})"
    case None => "None"
    case v: String => "\"" + v + "\""
    case i: Int => i.toString
  }

  private def arityToScala(markers: List[ArityComponent]): ArgumentList = {
    val as = markers.map {
      case SimpArg(n, _) => ("x" + n.abs, "Term", false)
      case LabelArg(n,_,_) => ("x" + n.abs, "OML", false)
      case ImplicitArg(n, _) => ("x" + n.abs, "Term", false)
      case SimpSeqArg(n, _, _) => ("xs" + n.abs, "List[Term]", true)
      case LabelSeqArg(n, _, _,_) => ("xs" + n.abs, "List[OML]", true)
      case Var(n, _, None, _) => ("v" + n, "VarDecl", false)
      case Var(n, _, Some(_), _) => ("vs" + n, "Context", true)
    }.map {x => Argument(x._1,x._2,x._3)}
    ArgumentList(as)
  }

  /** 1%w, */
  private val defaultNotation = TextNotation(Mixfix(List(SimpSeqArg(1, Delim(" "), CommonMarkerProperties.noProps))), Precedence.integer(0), None, false)
  /** additional lines (without indentation) to be added to the companion object, most importantly the apply/unapply methods */
  protected def companionObjectFields(c: Constant): List[String] = {
    val nt = c.not.getOrElse(defaultNotation)
    val arity = nt.arity
    if (arity.isApplication || arity.isPlainBinder || arity.isConstant) {
       val bindings = arityToScala(arity.variables)
       val args = arityToScala(arity.arguments)
       val op = new MMTOperator(c.path, bindings, args) // TODO this does generate correct unapply methods for sequence arguments
       op.methods
    } else {
      Nil
    }
  }
}
