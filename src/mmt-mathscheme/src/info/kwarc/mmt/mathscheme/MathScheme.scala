package info.kwarc.mmt.mathscheme

import info.kwarc.mmt.api.checking.ExtendedCheckingEnvironment
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.DeclaredModule
import info.kwarc.mmt.api.notations.{Marker, SimpArg}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

/*


/** Utility for starting the catalog and calling the Twelf compiler
  */
class MathScheme extends Compiler {
   def isApplicable(src: String) = src == "mathscheme"
   
   override def includeFile(n: String) : Boolean = n.endsWith(".msl")
   
   var path : File = null
   /** None: we're on unix
    *  Some("/cygdrive"): we're on windows using cygwin, so file paths are translated into cygwin mounts using a prefix */ 
   var prefix : Option[String] = None
   
   private def log(msg : => String) {report("mathscheme", msg)}
   
   /** 
    * creates and initializes a Catalog
    * first argument is the location of the twelf-server script
    */
   override def init(controller: Controller, args: List[String]) {
      super.init(controller, Nil)
      val p = args(0)
      if (p.startsWith("cygwin:")) {
         prefix = Some(p.substring(7,p.length))
         path = File(args(1))
      } else
         path = File(args(0))
   }

   /** 
     * Compile a MathScheme file to OMDoc
     * @param in the input Twelf file 
     * @param out the file in which to put the generated OMDoc
     */
   def compile(in: File, out: File) : List[SourceError] = {
      File(out.getParent).mkdirs
      def toCygwinIfNeeded(f: File) = prefix match {
         case None => f.toString
         case Some(p) => p + f.toString.replace(":", "").replace("\\", "/") //remove the ":" and turn the \'s into /'s; TODO: clean up this HACK 
      }
      val inCyg = toCygwinIfNeeded(in)
      val outCyg = toCygwinIfNeeded(out.setExtension("omdoc"))
      val procBuilder = new java.lang.ProcessBuilder(path.toString, in.toString, out.setExtension("omdoc").toString)
      //procBuilder.redirectErrorStream()
      val proc = procBuilder.start()
      Nil
   }
}

/*
object TwelfTest {
   def main(args: Array[String]) {
      val twelf = new Twelf(File("c:\\twelf-mod\\bin\\twelf-server.bat"))
      twelf.init
      twelf.addCatalogLocation(File("c:/Twelf/Unsorted/testproject/source"))
      //twelf.check(File("e:\\other\\twelf-mod\\examples-mod\\test.elf"), File(".")) 
      val errors = twelf.compile(File("c:/Twelf/Unsorted/testproject/source/test.elf"), File("c:/Twelf/Unsorted/testproject/source/test.omdoc"))
      println(errors.mkString("\n"))
      twelf.destroy
   }
}
*/ */

class Plugin extends frontend.Plugin {
   val theory = Path.parseM("http://test.org/mathscheme?Meta",NamespaceMap.empty)
   val dependencies = List("info.kwarc.mmt.lf.Plugin")
   override def start(args: List[String]) {
      val em = controller.extman
      // content enhancers
      em.addExtension(new Extends)
   }
}

class Extends extends StructuralFeature("extends") {
   def getHeaderNotation: List[Marker] = List(SimpArg(1))

   override val bodyDelim: String = "by"

   private def getDom(t : TermContainer) = t.get.get match {
      case OMMOD(p) => p
   }

   override def getInnerContext(dd: DerivedDeclaration): Context = Context.empty ++ getDom(dd.tpC)

   override def processHeader(header: Term) = header match {
      case OMA(OMMOD(`mpath`), List(t @ OMPMOD(p,_))) => (LocalName("EXTENDS_" + p.name), t)
   }
   override def makeHeader(dd: DerivedDeclaration) = OMA(OMMOD(`mpath`), dd.tpC.get.get :: Nil)

   def elaborate(parent: DeclaredModule, dd: DerivedDeclaration): Elaboration =
      new Elaboration {
         val dom = getDom(dd.tpC)
         val domain = LocalName(dom) :: dd.module.getDeclarations.map(_.name)
         def getO(name: LocalName): Option[Declaration] = name match {
            case LocalName(List(ComplexStep(`dom`))) => Some(PlainInclude(dom, parent.path))
            case n =>
               dd.module.getO(n) match {
                  case Some(c : Constant) => Some(Constant(parent.toTerm,c.name,c.alias,c.tp,c.df,c.rl,c.notC))
                  case Some(_) => ???
                  case None => None
               }

         }
      }

   def elaborateInContext(prev: Context, dv: VarDecl): Context = prev
   def checkInContext(prev: Context, dv: VarDecl): Unit = {}

   def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}
}