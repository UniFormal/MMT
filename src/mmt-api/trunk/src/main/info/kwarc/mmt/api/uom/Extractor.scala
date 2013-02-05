package info.kwarc.mmt.api.uom
import info.kwarc.mmt.api._
import frontend._
import modules._
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

case class ExtractError(s: String) extends Error(s)

object Extractor {

   private val base = DPath(utils.URI("http", "mmt.kwarc.info") / "openmath") ? "Base"
   private val fun = OMID(base ? "fun")
   private val seqfun = OMID(base ? "seqfun")
   
   /* Create unique package name for the OMDoc document */
   def UriToPackage(str : String) : String = {
     var result = "package "
     
     var packageName : java.lang.String = str
     packageName = packageName.trim
     /* remove protocol part */
     packageName = packageName.substring(packageName.indexOf("//") + 2)
     /* domain address part of URI */
     var webPart = packageName.substring(0 , packageName.indexOf("/"))
     /* local name in domain */
     var localPath = packageName.substring(packageName.indexOf("/") +1)

     /* print the reversed domain part to file  */
     var reverseBase = webPart.split("[.]")
     for (i <- 0 to reverseBase.length-1)
       result += (reverseBase(reverseBase.length -1 - i) + ".")

     /* in the local name part replace the / with . */
     localPath = localPath.replace('/', '.')

     result += (localPath + "\n")
     result
   }

   def getGlobalName(t: DeclaredTheory, c: Constant) : String = "base ? \"" + t.name + "\" ? \"" + c.name + "\""

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
   
   private val imports = """
import info.kwarc.mmt.api._ 
import objects._
import uom._
"""

   def doTheory(t: DeclaredTheory, out: java.io.PrintWriter) {
     println("Handling theory \"" + t.name  +  "\"")
     /* open and append */
     out.println(UriToPackage(t.parent.toString))
     out.println(imports)
     out.println("object " + t.name + " {")
     val baseUri = t.parent.uri
     out.println("  val base = DPath(utils.URI(\"" + baseUri.scheme.getOrElse("") + 
        "\", \""+ baseUri.authority.getOrElse("") +"\")" + 
        baseUri.path.foldRight("")((a,b) => " / \""+ a + "\"" + b) +
        ")"
     )
	  t.getDeclarations map {
	     case c: Constant =>
	       // called if this Constant is an implementation
          def funConst(args: List[Term], lastIsSeqArg: Boolean) {
               val implemented = args.head match {
                  case OMID(p) => "Path.parseS(\"" + p.toPath + "\")"
               }
               val normalArgs = if (lastIsSeqArg) args.length - 3 else args.length - 2
               var s = (normalArgs,lastIsSeqArg) match {
                  case (0,true)  => "Flexary"
                  case (1,true)  => "OneAndFlexary"
                  case (2,true)  => "TwoAndFlexary"
                  case (1,false) => "Unary"
                  case (2,false) => "Binary"
                  case (3,false) => "Ternary"
                  case _ => "Flexary"
               }
               out.println("  val " + c.name.last + " = " + s + "(" + implemented + ") {")
               /* Create the block marked by start end tags  */
               out.println("  // UOM start " + c.path.toString)
               c.df match {
                  case Some(Scala(code)) =>
                     out.println("    " + code)
                  case _ =>
                     out.println("    null")
               }
               out.println("  // UOM end")
               out.println("  }")
          }
          // called otherwise
	       def otherConst {
               out.println("  val " + c.name + " = "+ getGlobalName(t,c))
	       }
	       c.tp match {
	          case Some(term) => term match {
	             case OMA(this.fun, args) => funConst(args, false)
	             case OMA(this.seqfun, args) => funConst(args, true)
	             case _ => otherConst
	          }
	          case _ => otherConst
	       }
	      case _ => 
	   }
     println("Done with that theory\n\n")
     out.println("\n}\n")
   }

   def doDocument(controller: Controller, dpath: DPath, outFile: File) {
      val doc = controller.getDocument(dpath)
      var out = utils.File.Writer(outFile)
      out.println("//Source file generated by the Universal OpenMath Machine\n")
  	   doc.getModulesResolved(controller.globalLookup).foreach {
  	  	   case t : DeclaredTheory => doTheory(t, out)
  	  	   case _ => 
  	   }
      out.close    //TODO some exception handling
   }
}


