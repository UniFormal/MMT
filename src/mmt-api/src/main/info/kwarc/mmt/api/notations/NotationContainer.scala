package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import collection.mutable.ListMap

class NotationDimension {
   private var _notations = new ListMap[Int,List[TextNotation]]
   private var _maxArity = -1 //maximum arity of this notations, smaller ones imply partial applications

   def notations = _notations

   def isDefined = !notations.isEmpty

   def maxArity = _maxArity
   //default notation
   def default = get(Some(maxArity), None).headOption //assuming fully applied

   def get(arity : Option[Int] = None, lang : Option[String] = None) : List[TextNotation] = {
     var options = arity.map(notations.getOrElse(_, Nil)).getOrElse(notations.values.flatten).toList
     lang.foreach {l =>
       options = options.filter(_.scope.languages.contains(l))
     }
     NotationDimension.order(options)
   }

   def set(not : TextNotation): Unit = {
     val l = not.arity.length
     val nots = notations.getOrElse(l, Nil)
     notations(l) = (not :: nots).distinct // replaces previous occurrence of same notation (which might differ in metadata)
     if (l > maxArity) {
       _maxArity = l
     }
   }

   def update(nd : NotationDimension) : Boolean = {
      val changed = _notations != nd.notations // over-reports changes in the rare case where nothing but the order changed
      _notations = nd.notations
      _maxArity = nd.maxArity
      changed
   }
   def delete(): Unit = notations.clear()

  def mapInPlace(f: TextNotation => TextNotation): Unit = collectInPlace(not => Some(f(not)))

  def collectInPlace(f: TextNotation => Option[TextNotation]): Unit = {
    _notations.mapValuesInPlace((_, not) => {
      not.flatMap(f)
    })
    // TODO: recompute _maxArity?
  }
}

object NotationDimension {
  def order(notations : List[TextNotation]) : List[TextNotation] = {
     notations.sortWith((x,y) => x.scope.priority > y.scope.priority ||
                                (x.scope.priority == y.scope.priority && x.arity.length > y.arity.length))
  }
}

/** A NotationContainer wraps around various notations that can be associated with a Declaration */
class NotationContainer extends ComponentContainer {
   private val _parsingDim = new NotationDimension
   private val _presentationDim = new NotationDimension
   private val _verbalizationDim = new NotationDimension

   def parsingDim: NotationDimension = _parsingDim
   def presentationDim: NotationDimension = _presentationDim
   def verbalizationDim: NotationDimension = _verbalizationDim

   def parsing: Option[TextNotation] = parsingDim.default
   def presentation: Option[TextNotation] = presentationDim.default
   def verbalization: Option[TextNotation] = verbalizationDim.default

   /** get the notation for a certain component */
   def apply(c: NotationComponentKey) = c match {
      case ParsingNotationComponent => parsing
      case PresentationNotationComponent => presentation
      case VerbalizationNotationComponent => verbalization
   }
   /** set the notation for a certain component */
   def update(c: NotationComponentKey, tn: TextNotation): this.type = {
     c match {
       case ParsingNotationComponent => parsingDim.set(tn)
       case PresentationNotationComponent => presentationDim.set(tn)
       case VerbalizationNotationComponent => verbalizationDim.set(tn)
     }

     this
   }
   /** update all notations using the values of a different container */
   def update(c: ComponentContainer): Boolean = {
     c match {
       case nc: NotationContainer =>
         parsingDim.update(nc.parsingDim) ||
         presentationDim.update(nc.presentationDim) ||
         verbalizationDim.update(nc.verbalizationDim)
       case _ => throw ImplementationError("not a NotationContainer")
   }}
   def delete(): Unit = {
      parsingDim.delete()
      presentationDim.delete()
      verbalizationDim.delete()
   }
   
   /** adds all notations into the current container */
   def add(that: NotationContainer): this.type = {
      val comps = List(ParsingNotationComponent,PresentationNotationComponent,VerbalizationNotationComponent)
      comps.foreach {c =>
        (this.apply(c) orElse that.apply(c)).foreach {not =>
          update(c, not)
        }
      }
      this
   }
    
   /** a copy of this NotationContainer with some other notations merged in */
   def merge(that: NotationContainer): NotationContainer = {
      val ntMerged = copy()
      ntMerged.add(that)
      ntMerged
   }
   def copy(): NotationContainer = {
     val ntCopy = NotationContainer.empty()
     ntCopy.add(this)
     ntCopy
   }

   def isDefined = parsing.isDefined || presentation.isDefined || verbalization.isDefined
   def getComponents = parsing.toList.map(_ => ParsingNotationComponent(this)) :::
                       presentation.toList.map(_ => PresentationNotationComponent(this))
                       verbalization.toList.map(_ => VerbalizationNotationComponent(this))

   /** @return an appropriate notation for presentation, if any */
   def getPresentDefault : Option[TextNotation] = presentation orElse parsing orElse verbalization
   /** @return an appropriate notation for parsing, if any */
   def getParseDefault  : Option[TextNotation] = parsing
   /** @return an appropriate notation for verbalization, if any */
   def getVerbalDefault : Option[TextNotation] = verbalization
   def getNotations(dim : Option[Int], lang : Option[String] = None) : List[TextNotation] = {
     dim match {
       case None =>
         val all = parsingDim.get(None, lang) ::: presentationDim.get(None, lang) ::: verbalizationDim.get(None, lang)
         NotationDimension.order(all)
       case Some(1) => parsingDim.get(None, lang)
       case Some(2) => presentationDim.get(None, lang) ::: parsingDim.get(None, lang)
       case Some(3) => verbalizationDim.get(None, lang)
       case Some(n) => throw ImplementationError("Invalid notation dimension value " + n)
     }
   }

   def getAllNotations : List[TextNotation] = {
     List(parsingDim.notations.values.flatten,
          presentationDim.notations.values.flatten,
          verbalizationDim.notations.values.flatten
     ).flatten
   }

  def map(f: TextNotation => TextNotation): NotationContainer = copy().mapInPlace(f)
  def mapInPlace(f: TextNotation => TextNotation): this.type = collectInPlace(x => Some(f(x)))

  def collectInPlace(f: TextNotation => Option[TextNotation]): this.type = {
    _parsingDim.collectInPlace(f)
    _presentationDim.collectInPlace(f)
    _verbalizationDim.collectInPlace(f)
    this
  }

   def toNode = {
      val n1 = parsingDim.notations.values.flatten map {
         case n if ! metadata.Generated(n) =>
           utils.xml.addAttr(n.toNode, "dimension", "1")
         case _ => Nil
      }
      val n2 = presentationDim.notations.values.flatten map {
         case n if ! metadata.Generated(n) =>
           utils.xml.addAttr(n.toNode, "dimension", "2")
         case _ => Nil
      }
      val n3 = verbalizationDim.notations.values.flatten map {
         case n if ! metadata.Generated(n) =>
           utils.xml.addAttr(n.toNode, "dimension", "3")
         case _ => Nil
      }
      if (parsingDim.isDefined || presentationDim.isDefined || verbalizationDim.isDefined)
         <notations>{n1}{n2}{n3}</notations>
      else
         Nil
   }
   override def toString = {
      val s1 = parsing match {
         case Some(n) => " # " + n.toText
         case None => ""
      }
      val s2 = presentation match {
         case Some(n) => " ## " + n.toText
         case None => ""
      }
      //TODO .toString for verbalization notation
      s1 + s2
   }
}

object NotationContainer {
   def empty(): NotationContainer = new NotationContainer

   @deprecated("use NotationContainer.empty()", "v20.0.0")
   def apply(): NotationContainer = new NotationContainer
   def apply(tnOpt: Option[TextNotation]): NotationContainer = {
      val nc = NotationContainer.empty()
      tnOpt foreach {tn => nc.parsingDim.set(tn)}
      nc
   }
   def apply(tn: TextNotation): NotationContainer = apply(Some(tn))
   def apply(tn: TextNotation, tn2: TextNotation): NotationContainer = {
      val nc = apply(tn)
      nc.presentationDim.set(tn2)
      nc
   }
   def apply(tn: TextNotation, tn2: TextNotation, tn3: TextNotation): NotationContainer = {
      val nc = apply(tn, tn2)
      nc.verbalizationDim.set(tn3)
      nc
   }
   /** parses a notation element containing several notations of different dimensions (1-dimensional is default) */
   def parse(ns: scala.xml.NodeSeq, name : ContentPath): NotationContainer = {
      val nc = new NotationContainer
      ns foreach {c =>
         val tn = TextNotation.parse(c, NamespaceMap(name))
         utils.xml.attr(c, "dimension") match {
            case "1" | "" => nc.parsingDim.set(tn)
            case "2"      => nc.presentationDim.set(tn)
            case "3"      => nc.verbalizationDim.set(tn)
         }
      }
      nc
   }
}
