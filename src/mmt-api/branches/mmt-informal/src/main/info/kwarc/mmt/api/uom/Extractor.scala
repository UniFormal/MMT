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
import utils.FileConversion._


/** apply/unapply methods for the constructor Scala(code: String): Term to represent escaped Scala code in an MMT Term */
object Scala {
   val cd = DPath(utils.mmt.baseURI / "urtheories") ? "Scala"
   def apply(t: String) = OMSemiFormal(Text("scala", t))
   def unapply(t: Term) : Option[String] = t match {
      case OMSemiFormal(List(Text("scala", t))) => Some(t)
      case _ => None
   }
   def symbol(s: String) = OMS(cd ? s)
}

object ScalaLambda {
   val path = Scala.cd ? "Lambda"
   def apply(con: Context, t: Term) = OMBIND(OMID(this.path), con, t) 
   def unapply(t: Term) : Option[(Context,Term)] = t match {
      case OMBIND(OMID(this.path), con, t) => Some((con,t))
      case t => Some((Context(),t))
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
   private val keywords = List("true", "false", "type", "val", "var", "def", "class", "trait", "object", "extends", "with", "while", "do", "for")
   /** preused identifiers, i.e., declared in Object */
   private val reserved = List("eq", "List", "Set", "String")
   /** escapes strings to avoid clashes with Scala keywords */
   private def escape(s:String) = {
      if (keywords.contains(s))
        "`" + s + "`"
      else if (reserved.contains(s))
        "OM" + s
        else s
   }
   private def nameToScalaQ(p: GlobalName) = (p.module.toMPath.name.toPath + "_" + p.name.toPath).replace("/", "__")
   private def nameToScala(l: LocalName) = escape(l.toPath.replace("/","."))
   private def nameToScala(l: LocalPath) = escape(l.toPath.replace("/","."))
   /** package URI */
   def dpathToScala(d: DPath) = {
      val u = d.uri
      var auth = u.authority.getOrElse("").split("\\.").toList.reverse
      if (auth == List("")) auth = Nil
      (auth ::: u.path).mkString(".")
   }
   /** package URI . modname */
   def mpathToScala(m: MPath) = dpathToScala(m.doc) + "." + nameToScala(m.name) 
     
   private val imports = "import info.kwarc.mmt.api._\n" + "import objects._\n" + "import uom._\n" +
    "import ConstantScala._\n"


   private def arityToScala(arity: Arity) : List[(String,String)] = arity.components.map {
      case Arg(n) => ("x" + n.abs, "Term")
      case ImplicitArg(n) => ("x" + n.abs, "Term")
      case SeqArg(n,_) => ("xs" + n.abs, "List[Term]")
      case Var(n,_,None) => ("v" + n, "VarDecl")
      case Var(n,_,Some(_)) => ("vs" + n, "Context")
   }
   private def lastArgIsSeq(arity: Arity) = ! arity.arguments.isEmpty && arity.arguments.last.isSequence
   private def lastVarIsSeq(arity: Arity) = ! arity.variables.isEmpty && arity.variables.last.isSequence
   
   private def termToScala(t: Term)(implicit controller: Controller): String = t match {
      case OMA(f, args) =>
         val normalArgs = f match {
            case OMS(p) =>
               controller.globalLookup.getConstant(p).not.map {n =>
                  val a = n.getArity
                  a.arguments.length - (if (lastArgIsSeq(a)) 1 else 0)
               }.getOrElse(0)
            case _ => 0
         }
         val argsRecurse = args.map(termToScala)
         var argsScala = argsRecurse.take(normalArgs).mkString(", ")
         if (normalArgs > 0 && normalArgs < argsRecurse.length)
             argsScala += ", "
         if (normalArgs < argsRecurse.length)
             argsScala += s"List(${argsRecurse.drop(normalArgs).mkString(", ")})"
         s"${termToScala(f)}($argsScala)"
      case OMBIND(b, con, sc) => s"${termToScala(b)}(${contextToScala(con)}, ${termToScala(sc)})"
      case OMS(p)   => nameToScalaQ(p)
      case OMV(n)   => n.toPath
      case OMI(i)   => s"OMI(${i.toString})"
      case OMF(f)  	=> s"OMF(${f.toString})"
      case OMSTR(s) => "OMSTR(\"" + s + "\")"
      case t: OMSemiFormal => "(throw ParseError(\"\"\"informal term " + t.toString + "\"\"\"))"
   }
   // drops types, definiens
   private def contextToScala(c: Context): String = {
      val vars = c.variables.map("VarDecl(\"" + _.name + "\n,None,None)").mkString(", ") 
      s"Context($vars)"
   }
   
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
   
   private val OMFMP = DPath(utils.URI("http", "www.openmath.org") / "cd") ? "OpenMath" ? "FMP"
   def doTheory(controller: Controller, t: DeclaredTheory, out: java.io.PrintWriter): String = {
     val pack = dpathToScala(t.parent.doc)
     out.println("package " + pack)
     out.println(imports)
     // generating the trait
     val includes = t.getIncludesWithoutMeta.map(i => " with " + mpathToScala(i)).mkString("")
     out.println(s"trait ${nameToScala(t.name)} extends TheoryScala$includes {")
     t.getDeclarations foreach {
        case c: Constant =>
          if (c.tp == Some(OMID(OMFMP))) {
             val qname = nameToScalaQ(c.path)
             val qnameString = "\"" + qname + "\""
             out.println(s"  val $qname = _assert($qnameString, _ => ${termToScala(c.df.get)(controller)}, _ == logic1_true())\n")
          } else {
             val arity = c.not.map(_.getArity).getOrElse(Arity.constant)
             val scalaArgs = arityToScala(arity)
             val argtpString = scalaArgs.map(p => p._1 + ": " + p._2).mkString(", ")
             var o = s"  def ${nameToScalaQ(c.path)}($argtpString): Term\n"
             out.println(o)
          }
        case s: DeclaredStructure if ! s.name.isAnonymous =>
             // unnamed structures have been handled above already
             out.println("val " + nameToScalaQ(s.path) + ": " + mpathToScala(s.fromPath))
        case _ => 
     }
     out.println("}\n")
     // generating the auxiliary object
     out.println(s"object ${nameToScala(t.name)} extends TheoryScalaAux {")
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
     s"addTheory($pack.${nameToScala(t.name)})"
   }
   
   def doView(v: DeclaredView, from: DeclaredTheory, out: java.io.PrintWriter): String = {
     val pack = dpathToScala(v.parent.doc)
     out.println("package " + pack)
     out.println(imports)
     // generating the object
     val trtPackage = dpathToScala(from.path.parent)
     out.println("import " + trtPackage + "._\n")
     out.println("import " + nameToScala(from.path.name) + "._\n")
     val includes = from.getIncludesWithoutMeta.flatMap {f =>
        v.getO(LocalName(ComplexStep(f))) match {
           case Some(PlainViewInclude(_,_,i)) => " with " + mpathToScala(i)
           case _ => ""
        }
     }.mkString("")
     out.println(s"trait ${nameToScala(v.name)} extends ViewScala with ${nameToScala(from.path.name)}$includes {")
     var rules = ""
     from.getDeclarations foreach {
        case c: Constant =>
          if (c.tp != Some(OMID(OMFMP))) {
             val implemented = nameToScala(from.path.name) + "." + nameToScala(c.name) + ".path"
             val apath = v.path ? c.name
             val aO = v.getO(c.name)
             val arity = c.not.map(_.getArity).getOrElse(Arity.constant)
             val scalaArgs = arityToScala(arity)
             val defaultNames = scalaArgs.map(_._1)
             val varTypes = scalaArgs.map(_._2)
             val scalaName = nameToScalaQ(c.path)
             val (varNames, impl) = aO match {
                case None =>
                   if (arity.isConstant)
                      (defaultNames, s"OMS($implemented)") //constants are often not implemented
                   else
                      (defaultNames, "throw Unimplemented(\"" + scalaName + "\")")
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
             o += s"  // UOM start " + apath.toPath + "\n"
             o += s"  def $scalaName(${varNames.zip(varTypes).map(p => p._1 + ": " + p._2).mkString(", ")}) : Term = {\n"
             o += s"    $impl\n"
             o += s"  }\n"
             o += s"  // UOM end " + apath.toPath + "\n"
             val normalArgs = arity.arguments.length - (if (lastArgIsSeq(arity)) 1 else 0)
             var implConstr = Range(0,normalArgs).map(_ => "A").mkString("") + (if (lastArgIsSeq(arity)) "S" else "")
             if (implConstr == "") implConstr = "constant"
             rules += s"  declares(Implementation.$implConstr($implemented)(${nameToScalaQ(c.path)} _))\n"
             out.println(o)
          }
        case _ =>
     }
     out.println(rules)
     out.println("}\n")
     out.println(s"object ${nameToScala(v.name)} extends ${nameToScala(v.name)}\n")
     s"addView($pack.${nameToScala(v.name)})"
   }

   def doModule(controller: Controller, mod: Module, outFile: File): String = {
      var out = utils.File.Writer(outFile)
      out.println("//Source file generated by the Universal OpenMath Machine\n")
  	   val result = mod match {
  	  	   case t: DeclaredTheory =>
  	  	      doTheory(controller, t, out)
  	  	   case v: DeclaredView =>
  	  	      v.from match {
  	  	         case OMMOD(f) =>
  	  	            val dom = controller.globalLookup.getDeclaredTheory(f)
  	  	            doView(v, dom, out)
  	  	      }
  	  	   case _ => ""
  	   }
      out.close
      result
   }
   
   def doFolder(dpath: DPath, childResults: List[String], outFile: File): String = {
      var out = utils.File.Writer(outFile)
      val pack = dpathToScala(dpath)
      out.println("//Source file generated by the Universal OpenMath Machine\n")
      out.println(s"package $pack")
      out.println("import info.kwarc.mmt.api.uom._\n\n")
      out.println("object NAMESPACE extends DocumentScala {\n")
      childResults foreach {s =>
         out.println("    " + s + "\n")
      }
      out.println("}\n")
      out.close
      s"addDocument($pack.NAMESPACE)"
   }
}


