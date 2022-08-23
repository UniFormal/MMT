package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import modules._
import objects._
import libraries._
import notations._

/**
 * Declarations are the children of [[Module]]s.
 * 
 * They are mostly [[Constant]]s for the syntax and [[RuleConstant]]s for the semantics (= rule-based implementation).
 * The MMT data model is extensible via [[DerivedDeclaration]]s.
 * 
 * [[Structure]]s (except for includes) are conceptually a derived declaration but are hard-coded as a separate kind of declaration.
 * Inlcudes are represented as special cases of structures.
 */
abstract class Declaration extends ContentElement {
   /** to allow for sharper types of fields, every subclass of Declaration defines this to be itself */
   type ThisType >: this.type <: Declaration

   /** the containing module */
   lazy val parent = home.toMPath

   /** the containing document if there is a document between this declaration and the containing module */
   /* This is meant to be provided when constructing the declaration.
      But it is realized as a variable because it would be too tedious to add it as a constructor argument everywhere.
    */
   private var relDocHome_ = LocalName.empty
   def setDocumentHome(ln: LocalName): Unit = {
      relDocHome_ = ln
   }
   def relativeDocumentHome = relDocHome_
   def documentHome = parent.toDPath / relDocHome_
   override def merge(that: StructuralElement): Unit = {
      that match {
         case that: Declaration =>
           relDocHome_ = that.relDocHome_
         case _ => throw ImplementationError("not a declaration")
      }
   }

   /** the containing module
    *
    * this is almost always OMMOD(p:MPath),
    * the main exception are generated anonymous modules
    */
   val home : Term
   /** the local name in the containing module
    *
    *  for symbols: the name of the symbols
    *
    *  for assignments: the name of the symbols to which a value is assigned
    */
   val name : LocalName
   /** an alternative name
    *
    *  None by default; overridden in particular by Constant
    */
   def alternativeNames: List[LocalName] = Nil

   /** @return the shortest name and all other names */
   def primaryNameAndAliases = {
     val names = (name :: alternativeNames).sortBy(_.length)
     (names.head, names.tail)
   }

   /** the full MMT URI, parent ? name */
   def path = GlobalName(parent, name)
   /** the OMS referencing this declaration */
   def toTerm = OMS(path)

   // sharper type
   def getDeclarations: List[Declaration]

   /** a recursively translated copy of this declaration with a URI
    *  @param newHome the home theory of the result
    *  @param prefix the prefix used to form the name of the new declaration
    */
   def translate(newHome: Term, prefix: LocalName, translator: Translator, context : Context): ThisType
   /** a recursively translated copy of this declaration */
   def translate(translator: Translator, context : Context): ThisType = translate(home, LocalName.empty, translator, context)
   /** a new declaration with the same path obtained by replacing fields in 'this' with corresponding fields of 'that'
    *  Unfortunately, this must take any declaration and throw an error if 'not (that : ThisType)'
    */
   def merge(that: Declaration): ThisType

   /** called to throw an error from within 'merge' */
   protected def mergeError(that: Declaration): Nothing = throw GeneralError("cannot merge " + that.path + " into " + this.path)
}

/** declarations that have a notation */
trait HasNotation {
   def notC: NotationContainer
   def not = notC.parsing
   def notNode = notC.toNode
}

/** a [[Module]] as a [[Declaration]], i.e., inside some other module
 *  @param home the containing module
 *  @param name the name in that module
 *  @param mod the contained module
 *  invariant for non-induced NestedModule's: module.path == parent / name
 */
class NestedModule(val home: Term, val name: LocalName, mod: Module) extends Declaration with ModuleWrapper {
   type ThisType = NestedModule
   val feature = mod.feature
   def module = mod
   def translate(newHome: Term, prefix: LocalName, translator: Translator, context : Context): NestedModule = {
     val modT = mod.translate(utils.mmt.mmtbase, LocalName(newHome.toMPath/prefix)/name, translator,context)
     new NestedModule(newHome, prefix/name, modT)
   }
   //TODO this is where patterns (seen as defined theories) can be mapped to larger theories
   def merge(that: Declaration): NestedModule = {
     mergeError(that)
   }
}

trait HasType {
  def tpC: TermContainer
  def tp = tpC.get
  def tpNode = if (tp.isDefined) <type>{tp.get.toOBJNode}</type> else Nil
  def translateTp(translator: Translator, context : Context) = TermContainer(tp map {t => translator.applyType(context, t)})
}

trait HasDefiniens {
  def dfC: TermContainer
  def df = dfC.get
  def dfNode = if (df.isDefined) <definition>{df.get.toOBJNode}</definition> else Nil
  def translateDf(translator: Translator, context : Context) = TermContainer(df map {t => translator.applyDef(context, t)})
}

