package info.kwarc.mmt.uom
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.MyList._
import info.kwarc.mmt.api.objects._
import java.io._

object Extractor extends {
   val report = new frontend.FileReport(new java.io.File("uom.log"))
   val checker = new libraries.FoundChecker(libraries.DefaultFoundation)} 
      with Controller(checker, report) {

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
     return result
   }

   def getGlobalName(t: DeclaredTheory, c: Constant) : String = {
     return ( "GlobalName(OMMOD("  + "MPath(base, LocalPath(List(\n    \""
     + t.name + "\")))), LocalName(List(NamedStep(\"" + c.name + "\"))))"
     )
   }

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

   def doTheory(t: DeclaredTheory) {
     println("Handling theory \"" + t.name  +  "\"")
     /* open and append */
     var out = new PrintWriter(new BufferedWriter(
       new FileWriter("extracted.scala", true)))
     out.println(UriToPackage(t.parent.toString) + "{")
     out.println("class " + t.name + " {")
     out.println("  val base = DPath(new utils.xml.URI(\"" +
       t.parent.toString  + "\"))\n")
	   t.valueList map {
	      case c: Constant =>  // handle constants here
          c.df match {
            case Some(term) => { 
              term match {
                case OMFOREIGN(node) => {
                  /* Create the _* block, marked by start end tags  */
                  out.println("\n  // UOM start " + c.path.toString)
                  out.println("  "+node.text)
                  out.println("  // UOM end\n")

                  /* Create the auxilary _** block  */
                  val params =  getParameters(node.text)
                  out.print("  def " + c.name + "*(l : Term*) : Term "
                    + " = {\n    return " 
                    + c.name + "(")

                  // The parameters are of type Term*
                  if (params == -1) { out.print("l : _*") }
                  else {  // several parameters, none of which Term*
                    for (i <- 0 until params-1) {
                      out.print("l("+ i + "), ")
                    }
                    out.print("l("+ (params-1)  +")")
                  }

                  out.print(")\n  }\n\n"
                  )

                  /* Create the final Implementation block  */
                  out.println("  val " + c.name 
                    + "** = new Implementation(\n    "
                    + getGlobalName(t,c)
                    + "\n    ,\n    "
                    + c.name + "*\n    )\n"
                  )

                }
                case _ => out.println("  val " + c.name 
              + " = OMID("+ getGlobalName(t,c)  + ")")

              }
            }
            case None => out.println("  val " + c.name 
              + " = OMID("+ getGlobalName(t,c)  + ")")
          }
	      case PlainInclude(from, to) => {
          /* handle includes here */
          /* with the current implementation there is nothing to be done */

          //out.println("  val " + from.name.flat + " = new " + from.name.flat)
          //out.println("  " + UriToPackage(from.toString)
          //  .substring(8)
          //  .replace('.', '_')
          //  .replace('?', '_')
          //  )
          
        }
	      // case s: Structure => // TODO later
	      case _ => 
	   }
     println("Done with that theory\n\n")
     out.println("}\n}\n")
     out.close
   }

   def main(args: Array[String]) {
     if (args.length != 2) {
       println("The program should accept exactly 2 arguments")
       return;
     }

	   handle(Local)
	   val file = utils.xml.URI(new java.io.File(args(0)).toURI) // physical location of the file
	   handle(Read(DPath(file)))

     /* semantic identifier of the document (given by base attribute) */
	   val dpath = Path.parseD(args(1), base)	   
	   val doc = try {docstore.get(dpath)} // get the content of the document as a list of reference elements
	             catch {case NotFound(p) => println(p.toPath + " not found"); exit}
	   val theos : List[DeclaredTheory] =  // dereference all and keep the theories
	  	   doc.getItems mapPartial {r => get(r.target) match {
	  	  	   case t : DeclaredTheory => Some(t)
	  	  	   case _ => None
	  	   }
	   }

     var out = new PrintWriter(new BufferedWriter(new FileWriter("extracted.scala", false))); // open and overwrite
     out.println("//Source file generated by the Universal OpenMath Machine\n")

//     out.println(UriToPackage(args(1)))

     /* hangle initial imports */
     out.println("import info.kwarc.mmt.api._")
     out.println("import info.kwarc.mmt.api.objects._")
     out.println("import info.kwarc.mmt.uom.Implementation")

     out.close    //TODO some exception handling

	   theos foreach doTheory // handle all theories
   }
}


