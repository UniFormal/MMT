package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import frontend._
import utils._
import utils.MyList.fromList
import scala.xml.Node
import scala.collection.mutable.HashMap
/** A NotationKey identifies the situation when a notation should be applied.
 * @param path an MMTURI identifying the applicable expressions, applicable to all expressions if omitted
 * @param role the syntactical position where the notation is applicable
 */
case class NotationKey(path : Option[Path], role : Role)

/** The NotationStore holds all styles and notations.
 * @param mem the memory
 * @param report the logging handler
 */
class NotationStore(mem : ROMemory, report : frontend.Report) {
   private val lib = mem.content
   protected val sets = new HashMap[MPath, Style]
   protected val defaults = new HashMap[NotationKey, Notation]

   /** the imports relations between notation sets */
   protected val imports = new ReflTransHashRelation[MPath]
   /** caches the list of imported notation sets */
   protected var visible = new HashMap[MPath, List[MPath]]
   private def log(s : => String) = report("notations", s)
   def get(path : MPath) : Style = {
      try {sets(path)}
      catch {case _ => 
         log("retrieving notation set " + path)
         throw frontend.NotFound(path)
      }
   }
   def getDefaults = defaults.valuesIterator
   /**
    * looks up a notation in a style by a key
    * 
    * @param path the notation set to be looked in
    * @param key the key for which the notation is looked up
    * @return the applicable notation, see https://svn.kwarc.info/repos/kwarc/rabe/Scala/doc/mmt.pdf
    * @throws PresentationError if notation found
    */
   def get(path : MPath, key : NotationKey) : Notation = {
      log("looking up notation for " + key)
      report.indent
      val nset = try {sets(path)}
                 catch {case _ => throw frontend.NotFound(path)}
     // look up key in defaults
     def getDefault(key : NotationKey) : Option[Notation] = {
         log("looking in defaults for key " + key)
         val not = key.path flatMap {
	         case p : MPath => try {sets(p).get(key)} catch {case _ => None}
	         case p : GlobalName if key.role.bracketable =>
	           //get default notation, ...
	           //defaults.get(key)
	           lib.getO(p) match {
	             case Some(c : symbols.Constant) => 
	               c.not orElse 
                   //... otherwise, if symbol arose from structure, get notation from preimage
                   (lib.preImage(p) flatMap (q => getDefault(NotationKey(Some(q), key.role))))
	               
	             case _ => None
	           }
	         case _ => None
	     }
         if (not.isDefined) log("found")
         not
      }

      //look up key in all visible sets, return first found notation
      def getDeep(key : NotationKey) : Option[Notation] = {
         //a list containing all imported sets in depth-first order
         //value is cached in visible
         log("looking in style    for key " + key)
         val vis = visible.getOrElseUpdate(path,imports.preimageDFO(path))
         val not = vis.mapFind(get(_ : MPath).get(key))
         //if applicable, lookup default notation from module
         if (not.isEmpty && key.path.isDefined && get(path).defaults == ImportsDefaults)
            getDefault(key)
         else {
            log("found")
            not
         }
      }
      val not = key.path match {
        case None =>
           getDeep(key) match {
              case Some(hn) => hn
              case None => 
                 throw GetError("no notation found for " + key + " in style " + path)
           }
        case Some(p) =>
           val r = key.role
           //to find the default notation highNot, we use keys without a path and try some roles in order
           val roles : List[Role] = r match {
              case Role_application(None) => 
                mem.ontology.getType(p) match {
                    case Some(ontology.IsConstant(rl)) => List(Role_application(rl), r) // application of constants may differ depending on their role, if given 
                    case _ => List(r)
                 }
              case Role_Constant(Some(s)) => List(r, Role_Constant(None))
              case r => List(r)
           }
           val highNot = roles mapFind {r => getDeep(NotationKey(None, r))} 
           val lowNot = getDeep(key) 
           (highNot, lowNot) match {
              case (Some(hn), None) => hn
              case (None, Some(ln)) => ln
              case (Some(hn), Some(ln)) =>
                 log("merging notations")
                 // the default notation may force being wrapped around lowNot
                 if (hn.wrap) SimpleNotation(ln.nset, ln.key, hn.pres.fill(ln.pres), hn.oPrec, false)
                 else ln
              case (None, None) =>
                 throw GetError("no notation found for " + key + " in style " + path)
           }
        }
      report.unindent
      log("notation found for " + key + ": " + not)
      not
   }
   /** adds a style, style import, or notation */
   def add(e : PresentationElement) {
      e match {
         case e : Style =>
            sets(e.path) = e
            mem.ontology += ontology.IsStyle(e.path)
            mem.ontology += ontology.HasDomain(e.path, e.from)
            mem.ontology += ontology.HasCodomain(e.path, e.to)
         case e : NotationImport =>
            imports += (e.from, e.to)
            mem.ontology += ontology.Includes(e.to, e.from)
            visible = scala.collection.mutable.HashMap.empty
         case e : Notation =>
            if (sets.isDefinedAt(e.nset))
               sets(e.nset).add(e)
            else {
               e.key.path match {
                  case Some(p : MPath) if p == e.nset => defaults(e.key) = e
                  case Some(objects.OMMOD(p) % _) if p == e.nset => defaults(e.key) = e
                  case _ => throw AddError("notation set " + e.nset + " does not exist, and default notations must be for containing theory: " + e.toString)
               }
            }
      }
   }
   /** deletes a style */
   def delete(m: MPath) {
      sets -= m
   }
   /** empties the store */
   def clear {
      imports.clear
      defaults.clear
      sets.clear
      visible.clear
   }
}