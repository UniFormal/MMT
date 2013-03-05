package info.kwarc.mmt.api.uom
import info.kwarc.mmt.api._
import frontend._
import modules._
import parser._
import libraries._
import documents._
import symbols._
import utils._
import objects._

/** apply/unapply methods for the constructor Scala(code: String): Term to represent escaped Scala code in an MMT Term */
object Scala {
   def apply(t: String) = OMSemiFormal(Text("scala", t))
   def unapply(t: Term) : Option[String] = t match {
      case OMSemiFormal(List(Text("scala", t))) => Some(t)
      case _ => None
   }
}

object ScalaLambda {
   val cd = DPath(utils.mmt.baseURI / "foundations") ? "Scala"
   val path = cd ? "lambda"
   def unapply(t: Term) : Option[(Context,Term)] = t match {
      case OMBIND(OMID(this.path), con, t) => Some((con,t))
      case _ => None
   }
}

case class ExtractError(s: String) extends Error(s)

object Extractor {

   private val base = DPath(utils.URI("http", "mmt.kwarc.info") / "openmath") ? "Base"

   //def getGlobalName(t: DeclaredTheory, c: Constant) : String = "_base ? \"" + t.name + "\" ? \"" + c.name + "\""

   def getParameters(str : String) : Int = {
     var braces = 0
     var colons = 0
     val start = str.indexOf('(')
     var numberOfSequence = 0

     for (i <- start until str.length) {
       val tmp = str.charAt(i)
       if ( tmp == '(') { braces = braces + 1 }
       if ( tmp == ')') { braces = braces - 1 } 
       if ( braces < 0 ) {
         System.err.println("Extractor: Inbalanced string found"+
           " in implementation")
         System.exit(1)
       }
       if ( braces == 0 ) {
         if (numberOfSequence > 1) {
           System.err.println("Extractor: Implementation has 2 parameters" +
             " of type Term*")
         }

         if (numberOfSequence == 0) {return colons}

         if ( (numberOfSequence == 1) && (colons > 1) ) {
           System.err.println("Extractor: Implementation has parameters" +
             " of both types Term* and Term"
           )
         }
         return -1
       }
       if ( tmp == ':'  ) { colons += 1 }
       if ( tmp == '*' ) { numberOfSequence += 1 }
     }
     System.err.println("Extractor: Inbalanced string found in code")
     System.exit(1)
     return -2
   }
   /** reserved identifiers */
   private val keywords = List("true", "false")
   /** preused identifiers, i.e., declared in Object */
   private val reserved = List("eq")
   /** escapes strings to avoid clashes with Scala keywords */
   private def escape(s:String) = {
      if (keywords.contains(s))
        "`" + s + "`"
      else if (reserved.contains(s))
        "om_" + s
        else s
   }
   private def nameToScala(l: LocalName) = escape(l.toPath.replace("/","."))
   private def nameToScala(l: LocalPath) = escape(l.toPath.replace("/","."))
   /** package URI */
   private def dpathToScala(d: DPath) = {
      val u = d.uri
      var auth = u.authority.getOrElse("").split("\\.").toList.reverse
      if (auth == List("")) auth = Nil
      (auth ::: u.path).mkString(".")
   }
   /** package URI . modname */
   private def mpathToScala(m: MPath) = dpathToScala(m.doc) + "." + nameToScala(m.name) 
     
   private val imports = "import info.kwarc.mmt.api._\n" + "import objects._\n" + "import uom._\n"

   private def arityToScala(arity: Arity) : List[(String,String)] = arity.components.map {
      case Arg(n) => ("x" + n.abs, "Term")
      case ImplicitArg(n) => ("x" + n.abs, "Term")
      case SeqArg(n,_) => ("xs" + n.abs, "List[Term]")
      case Var(n,_,None) => ("v" + n, "VarDecl")
      case Var(n,_,Some(_)) => ("vs" + n, "Context")
   }
   private def lastArgIsSeq(arity: Arity) = ! arity.arguments.isEmpty && arity.arguments.last.isSequence
   private def lastVarIsSeq(arity: Arity) = ! arity.variables.isEmpty && arity.variables.last.isSequence
   
   private def applyMethods(arity: Arity) : String = {
     val scalaArgs = arityToScala(arity)
     // x1 :: ... :: xn :: Nil or x1 :: ... :: xsn
     var argListString   = scalaArgs.map(_._1).mkString(" :: ")
     if (! lastArgIsSeq(arity))
        argListString = argListString + ":: Nil"
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
     else if (arity.isBinder)
         s"    def apply(vs1: Context, s2: Term) = OMBIND(OMID(this.path), vs1, s2)\n"
     else
         "  // no apply method generated for this arity\n"
     // def unapply(t: Term): Option[...] = ...
     val unapp = if (arity.isApplication)
         s"    def unapply(t: Term): Option[$tpString] = t match {\n" +
         s"      case OMA(OMID(this.path), $argListString) => Some($argTupleString)\n" +
         s"      case _ => None\n" +
         s"    }\n"
     else if (arity.isBinder)
         s"    def unapply(t: Term): Option[(Context, Term)] = t match {\n" +
         s"      case OMBIND(OMID(this.path), vs1, s2) => Some((vs1, s2))\n" +
         s"      case _ => None\n" +
         s"    }\n"
     else
         "  // no unapply methods generated for this arity\n"
     
     app + unapp
   }
   
   def doTheory(t: DeclaredTheory, out: java.io.PrintWriter) {
     /* open and append */
     out.println("package " + dpathToScala(t.parent.doc))
     out.println(imports)
     // generating the trait
     out.println(s"trait ${t.name} {")
     t.getDeclarations foreach {
        case c: Constant =>
          val arity = c.not.map(_.getArity).getOrElse(Arity.plainApplication)
          val scalaArgs = arityToScala(arity)
          val argtpString = scalaArgs.map(p => p._1 + ": " + p._2).mkString(", ")
          var o = s"  def ${nameToScala(c.name)}($argtpString): Term\n"
          out.println(o)
        case _ =>
     }
     out.println("}\n")
     // generating the auxiliary object
     out.println(s"object ${t.name} extends TheoryScala {")
     val baseUri = t.parent.uri
     out.println("  val _base = DPath(utils.URI(\"" + baseUri.scheme.getOrElse("") + 
        "\", \""+ baseUri.authority.getOrElse("") +"\")" + 
        baseUri.path.foldRight("")((a,b) => " / \""+ a + "\"" + b) +
        ")"
     )
     out.println("  val _path = _base ? \"" + t.name + "\"")
     t.getDeclarations foreach {
	     case c: Constant =>
	         var o = ""
	         o +=  s"\n  object ${nameToScala(c.name)} extends ConstantScala {\n"
	         o +=  s"    val parent = _path\n"
	         o +=   "    val name = \"" + c.name + "\"\n"
	         c.not foreach {n =>
	            val a = n.getArity
	            o += applyMethods(a)
	         }
	         o += "  }\n"
	         out.println(o)
	      case _ => 
	   }
     out.println("\n}\n")
   }
   
   def doView(v: DeclaredView, from: DeclaredTheory, out: java.io.PrintWriter) {
     out.println("package " + dpathToScala(v.parent.doc))
     out.println(imports)
     // generating the object
     val trt = mpathToScala(from.path)
     out.println(s"object ${v.name} extends ViewScala with $trt {")
     var rules = ""
     from.getDeclarations foreach {
        case c: Constant =>
          val apath = v.path ? c.name
          val aO = v.getO(c.name)
          val arity = c.not.map(_.getArity).getOrElse(Arity.plainApplication)
          val scalaArgs = arityToScala(arity)
          val defaultNames = scalaArgs.map(_._1)
          val varTypes = scalaArgs.map(_._2)
          val (varNames, impl) = aO match {
             case None =>
                   (defaultNames, "null")
             case Some(a: ConstantAssignment) => a.target match {
                case Some(ScalaLambda(con, Scala(s))) =>
                   (con.variables.map(_.name.toPath), s)
                case _ =>
                   (defaultNames, " //unexpected assignment in MMT view")
             }
             case Some(_) =>
                   (defaultNames, " //unexpected assignment in MMT view")
          }
          var o = ""
          o += s"  def ${nameToScala(c.name)}(${varNames.zip(varTypes).map(p => p._1 + ": " + p._2).mkString(", ")}) : Term = {\n"
          o += s"    // UOM start " + apath.toPath + "\n"
          o += s"    $impl\n"
          o += s"    // UOM end " + apath.toPath + "\n"
          o += s"  }\n"
          val normalArgs = arity.arguments.length - (if (lastArgIsSeq(arity)) 1 else 0)
          val implConstr = Range(0,normalArgs).map(_ => "A").mkString("") + (if (lastArgIsSeq(arity)) "S" else "")
          val implemented = trt + "." + c.name.last + ".path"
          rules += s"  declares(Implementation.$implConstr($implemented)(${c.name.last} _))\n"
          out.println(o)
        case _ =>
     }
     out.println(rules)
     out.println("}\n")
   }

   def doDocument(controller: Controller, dpath: DPath, outFile: File) {
      val doc = controller.getDocument(dpath)
      var out = utils.File.Writer(outFile)
      out.println("//Source file generated by the Universal OpenMath Machine\n")
  	   doc.getModulesResolved(controller.globalLookup).foreach {
  	  	   case t : DeclaredTheory => doTheory(t, out)
  	  	   case v: DeclaredView =>
  	  	      v.from match {
  	  	         case OMMOD(f) =>
  	  	            val dom = controller.globalLookup.getDeclaredTheory(f)
  	  	            doView(v, dom, out)
  	  	      }
  	   }
      out.close    //TODO some exception handling
   }
}


