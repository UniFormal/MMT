package info.kwarc.mmt.mizar.objects

import scala.collection.mutable._
//import scala.collection.immutable._
/**
 * Trait for all Mizar classes
 */
trait MizAny {
  var sreg : Option[SourceRegion] = None
}

/**
 * Class for Parser errors
 */
class MizErr() extends MizAny

object ParsingController {
   //var vocabulary : List[String] = Nil
   val dictionary : Dictionary = new Dictionary(Nil)

   var selectors : Map[String,Map[Int,Tuple2[Int,Int]]] = new HashMap()
   val attributes : Map[String,Map[Int,Int]] = new HashMap()
   //var definitions : LinkedList[XMLDefinition] = new LinkedList()
   var elems : LinkedList[MizAny] = new LinkedList()
   var defBlockCounter = 0
   var definiens : HashMap[Int,LinkedList[XMLDefiniens]] = HashMap()
   var deftheorems : HashMap[Int,LinkedList[MizDefTheorem]] = HashMap()
   //var currentDefBlock : Option[XMLDefinitionBlock] = None
   //var currentArticle : Option[MizArticle] = None
   var currentAid = ""

    /*
   def setArticle(aid : String) {
      currentArticle = Some(new MizArticle(aid, Nil))
      currentAid = aid
   }
   */
   def addToArticle(el : MizAny) : Unit = {
     elems = elems :+ el
   }

   def addDefiniens(d : XMLDefiniens)  = {
     val defBlockNr = defBlockCounter - 1 //they belong to the previous def block
     definiens(defBlockNr) :+= d
   }

   def addDefTheorem(dt : MizDefTheorem) = {
     val defBlockNr = defBlockCounter - 1 //they belong to the previous def block
     deftheorems(defBlockNr) :+= dt
   }

   def addDefinitionBlock(db : XMLDefinitionBlock) = {
     deftheorems(defBlockCounter) = LinkedList() //deftheorems will be added as they follow the def block
     definiens(defBlockCounter) = LinkedList() //definiens will be added as they follow the def block
     defBlockCounter += 1
     elems = elems ++ db.defs
   }

   def buildArticle() : MizArticle = {
     val art = new MizArticle(currentAid, Nil)
     elems.map({
       case d : XMLDefinition =>
         art.addElem(buildDefinition(d, art))
       case x =>
         art.addElem(x)
     })

     elems = new LinkedList()
     definiens = HashMap()
     deftheorems = HashMap()
     art
   }

   def buildDefinition(d : XMLDefinition, art : MizArticle) : MizDefinition = {
     //getting matching definiens (0 or 1)
     val (matchedDfns, unmatchedDfns) = definiens(d.defBlockNr).partition(x => (
         x.constraid == d.defaid &&
         x.constrkind == d.constrkind &&
         x.absconstrnr == d.defnr))

     //assert(matchedDfns.length <= 1, "ParsingController.buildDefinition, found more than one " +
     //      "(" + matchedDfns.length + ") matching definiens for " + d.defaid + d.constrkind + d.defnr)

     d.setDefiniens(matchedDfns.headOption)
     val leftOvers = if (matchedDfns.length > 1) matchedDfns.tail else Nil

     definiens(d.defBlockNr) = unmatchedDfns ++ leftOvers
     //getting matching def theorems (0 to *)
     val (matchedDts, unmatchedDts) = deftheorems(d.defBlockNr).partition(x => ( x.aid == d.aid &&
                                                      x.constrkind == d.constrkind &&
                                                      x.constrnr == d.constrnr))
     deftheorems(d.defBlockNr) = unmatchedDts //removing used deftheorems
     val dts = matchedDts.toList //matched def theorems will be added to the constructed definition

     val args = d.args.getOrElse(throw new Error("Controller.buildDefinition : no args found"))
     val prefix = d.isRedef match {
       case true => "Redef_" + d.kind + d.patternNr + "_of_" + d.defaid + "_"
       case false => ""
     }

     d.definiens match {
       case Some(di : XMLIsDefiniens) =>
         d.constrkind match {
           case "R" => new MizPredIsDef(d.name, d.defaid, d.defnr, args, di.cases, di.term, dts, prefix)
           case "K"|"O" => new MizFuncIsDef(d.name, d.defaid, d.constrkind, d.defnr, args, d.retType, di.cases, di.term, dts, prefix)
           case "M" => new MizModeIsDef(d.name, d.defaid, d.defnr, args, d.retTypeO, di.cases, di.term, dts, prefix)
           case "V" => new MizAttrIsDef(d.name, d.defaid, d.defnr, args, d.retType, di.cases, di.term, dts, prefix)
         }
       case Some(dm : XMLMeansDefiniens) =>
         d.constrkind match {
           case "R" => new MizPredMeansDef(d.name, d.defaid, d.defnr, args, dm.cases, dm.form, dts, prefix)
           case "K"|"O" => new MizFuncMeansDef(d.name, d.defaid, d.constrkind, d.defnr, args, d.retType, dm.cases, dm.form, dts, prefix)
           case "M" => new MizModeMeansDef(d.name, d.defaid, d.defnr, args, d.retTypeO, dm.cases, dm.form, dts, prefix)
           case "V" => new MizAttrMeansDef(d.name, d.defaid, d.defnr, args, d.retType, dm.cases, dm.form, dts, prefix)
         }
       case None => //expandable mode
         if (d.isExp) {
           new MizExpMode(d.name, d.aid, d.patternNr, args, d.getExpansion, dts)
         } else if (d.isSuperfluous) {//superfluous redefinition
           val args = d.args.getOrElse(throw new Error("Controller.buildDefinition for superfluous def and no args found")).map(_._2)

           //constructing definiens
           val vars = for ((tp,i) <- args.zipWithIndex) yield new MizLocusVar(i + 1)
           val term = new MizFunc(d.defaid, d.constrkind, d.defnr, vars)
           val df = new XMLIsDefiniens(d.aid, 0, d.defaid, d.constrkind, d.defnr, Nil, Some(term), args)
           definiens(d.defBlockNr) :+= df //adding reconstructed definiens
           buildDefinition(d, art) //re-calling build
         } else {
           throw new Error("Controller.buildDefinition no definiens found, and not expandable mode" + d.defaid + d.constrkind + d.defnr)
         }
       case _ => throw new Error("Controller.buildDefinition: invalid definiens found " + d.toString)
     }
   }

   def resolveDef(aid : String, kind : String, absnr : Int) : Option[String] = {
      dictionary.formats.find(x => ((x.aid == Some(aid)) && (x.kind == kind) && (x.absnr == absnr))) match {
        case Some(f) =>
          if (f.symbol == null){
            None
          } else {
            f.rightsymbol match {
              case Some(rightsymbol) => f.symbol.name + rightsymbol.name
              case None =>  f.symbol.name
            }
          }
          None //TODO remove for notations
        case None =>
          None
      }
   }

   def resolveVar(Svid : String) : Option[String] = {
      try {
         val vid = Svid.toInt
         dictionary.symbols.find(x => x.nr == vid && (x.kind == "I")) match {
            case Some(x) => Some(x.name)
            None //TODO remove for notations
            case None =>
            None
         }
      } catch {
         case e : Exception =>
         None
      }
   }

   def resolveSelector(aid : String, absnr : Int) : String = {
     "a"
   }

}
