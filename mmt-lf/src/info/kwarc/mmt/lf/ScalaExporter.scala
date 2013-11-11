package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._

import uom.GenericScalaExporter._

class ScalaExporter extends archives.FoundedExporter(LF._path, uom.Scala._path) with uom.GenericScalaExporter {
   val outDim = "lf-scala"
   val key = "lf-scala"
   override val packageSep = List("lf")
   
   private def typeEras(t: Term): (List[GlobalName],GlobalName) = t match {
      case OMS(a) => (Nil, a)
      case ApplySpine(OMS(a),_) => (Nil, a)
      case FunType(args, ret) if ! args.isEmpty =>
         val argsE = args.map {case (_,t) => typeEras(t)._2}
         val retE = typeEras(ret)._2
         (argsE, retE) 
   }
   
   private def typeToScala(t: Term) : String = t match {
      case OMS(a) => nameToScalaQ(a) + ".univ"
      case ApplySpine(OMS(a), _) => typeToScala(OMS(a))
      case FunType(args, ret) if ! args.isEmpty =>
         val argsS = args.map(a => typeToScala(a._2))
         val retS = typeToScala(ret)
         argsS.mkString("(", " => ", " => ") + retS + ")"
      case _ => "error"
   }
      
   def doCoveredTheory(t: DeclaredTheory) {
      outputHeader(t.parent.doc)
      outputTrait(t){c =>
         c.tp match {
            case None => ""
            case Some(tp) =>
               val synName = nameToScala(t.name) + "." + nameToScala(c.name) + ".path"
               val semName = nameToScalaQ(c.path)
               val Some((args,ret)) = FunType.unapply(tp)
               if (ret == Univ(1)) {
                  //scalaType(c.path)
                  val ini = s"  declares($synName)($semName)"
                  val decl = scalaVal(c.path, "RealizedType")
                  decl + "\n" + ini + "\n"
               } else {
                  // create declares(RealizedOperator(name)(argType1, ..., argTypeN, retType)(function))
                  val (argsE, retE) = typeEras(tp)
                  val lts = (argsE ::: List(retE)).map(nameToScalaQ).mkString(", ")
                  val dcl = s"RealizedOperator($synName, $lts)($semName)"
                  val ini = "  declares(" + dcl + ")"
                  // create def name(x0: argType1._univ, ..., xN: argTypeN._univ): retType.univ
                  val names = args.zipWithIndex.map {
                     case ((Some(n), _), _) => n.toPath
                     case ((None   , _), i) => "x" + i.toString
                  }
                  val argsES = argsE.map(a => nameToScalaQ(a) + ".univ")
                  val decl = scalaDef(c.path, names zip argsES, nameToScalaQ(retE) + ".univ")
                  decl + "\n" + ini + "\n"
                  /*
                   val argsS = args.zipWithIndex.map {
                     case ((nOpt, t), i) => 
                        val tS = typeToScala(t)
                        val nS = nOpt match {
                           case Some(n) => n.toPath
                           case None => "x" + i.toString
                        }
                        (nS, tS)
                  }
                  val df = scalaDef(c.path, argsS, typeToScala(ret))
                  */
               }
         }
      }
      outputCompanionObject(t){c => ""}
   }
   
   def doFunctor(v: DeclaredView) {}

   def doRealization(v: DeclaredView) {}
}