package info.kwarc.mmt.api.presentation
import jomdoc._
import jomdoc.utils._
import jomdoc.utils.MyList.fromList
import scala.xml.Node

/** A NotationKey identifies the situation when a notation should be applied.
 * @param path an MMTURI identifying the applicable expressions, applicable to all expressions if omitted
 * @param role the syntactical position where the notation is applicable
 */
case class NotationKey(path : Option[Path], role : Role)

/** The NotationStore holds all styles and notations.
 * @param lib the content store
 * @param depstore the abox store
 * @param report the logging handler
 */
class NotationStore(lib : libraries.Lookup, depstore : ontology.ABoxStore, report : frontend.Report) {
   protected val sets = new scala.collection.mutable.HashMap[MPath, Style]
   protected val defaults = new scala.collection.mutable.HashMap[NotationKey, Notation]

   /** the imports relations between notation sets */
   protected val imports = new ReflTransHashRelation[MPath]
   /** caches the list of imported notation sets */
   protected var visible = new scala.collection.mutable.HashMap[MPath, List[MPath]]
   private def log(s : => String) = report("notations", s)
   def get(path : MPath) : Style = {
      try {sets(path)}
      catch {case _ => 
         log("retrieving notation set " + path)
         throw frontend.NotFound(path)
      }
   }
   /**
    * looks up a notation in a style by a key
    * 
    * @param path the notation set to be looked in
    * @param key the key for which the notation is looked up
    * @return the applicable notation, see https://svn.kwarc.info/repos/kwarc/rabe/Scala/doc/mmt.pdf
    * @throws PresentationError if notation found
    */
   def get(path : MPath, key : NotationKey) : Notation = {
      val nset = try {sets(path)}
                 catch {case _ => throw frontend.NotFound(path)}
     // look up key in defaults
     def getDefault(key : NotationKey) : Option[Notation] = {
         key.path match {
             case None => None
	         case Some(_ : DPath) => None
	         case Some(p : MPath) => try {sets(p).get(key)} catch {case _ => None}
	         case Some(p : SPath) =>
	            //get default notation, ...
	            defaults.get(key) orElse
                   //... otherwise, if symbol arose from structure, get notation from preimage
                   (lib.preImage(p) flatMap (q => getDefault(NotationKey(Some(q), key.role))))
	      }
      }

      //look up key in all visisble sets, return first found notation
      def getDeep(key : NotationKey) : Option[Notation] = {
         //a list containing all imported sets in depth-first order
         //value is cached in visible
         val vis = visible.getOrElseUpdate(path,imports.preimageDFO(path))
         val not = vis.mapFind(get(_ : MPath).get(key))
         //if applicable, lookup default notation from module
         if (not.isEmpty && get(path).defaults == ImportsDefaults) {
            log("trying default notation")
            getDefault(key)
         } else
            not
      }
      val not =
        if (key.path.isEmpty)
           getDeep(key) match {
              case Some(hn) => hn
              case None => 
                 throw PresentationError("no notation found for " + key + " in notation set " + path)
           }
        else {
           val highNot = getDeep(NotationKey(None, key.role))
           val lowNot = getDeep(key) 
           (highNot, lowNot) match {
              case (Some(hn), None) => hn
              case (None, Some(ln)) => ln
              case (Some(hn), Some(ln)) =>
                 log("merging notations")
                 if (hn.wrap) SimpleNotation(ln.nset, ln.key, hn.pres.fill(ln.pres), hn.oPrec, false)
                 else ln
              case (None, None) =>
                 throw PresentationError("no notation found for " + key + " in notation set " + path)
           }
        }
      log("notation found for " + key + ": " + not)
      not
   }
   /** adds a style, style import, or notation */
   def add(e : PresentationElement) {
      e match {
         case e : Style =>
            sets(e.path) = e
            depstore += ontology.IsStyle(e.path)
            depstore += ontology.HasDomain(e.path, e.from)
            depstore += ontology.HasCodomain(e.path, e.to)
         case e : NotationImport =>
            imports += (e.from, e.to)
            depstore += ontology.HasOccurrenceOfInImport(e.to, e.from)
            visible = scala.collection.mutable.HashMap.empty
         case e : Notation =>
            if (sets.isDefinedAt(e.nset))
               sets(e.nset).add(e)
            else {
               e.key.path match {
                  case Some(p : MPath) if p == e.nset => defaults(e.key) = e
                  case Some(p : SPath) if p.parent == e.nset => defaults(e.key) = e
                  case _ => throw AddError("notation set " + e.nset + " does not exist, and default notations must be for containing theory: " + e.toString)
               }
            }
      }
   }
   /** empties the store */
   def clear {
      imports.clear
      defaults.clear
      sets.clear
      visible.clear
   }
}
