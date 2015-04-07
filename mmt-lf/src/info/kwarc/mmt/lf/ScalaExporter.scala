
package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._

import uom.GenericScalaExporter._

//TODO translate LF definitions to Scala definitions  

class ScalaExporter extends archives.FoundedExporter(LF._path, uom.Scala._path) with uom.GenericScalaExporter {
   val key = "lf-scala"
   override val packageSep = List("lf")
   
   private object IllFormed extends Throwable
   private def typeEras(t: Term): (List[GlobalName],GlobalName) = t match {
      case OMS(a) => (Nil, a)
      case ApplySpine(OMS(a),_) => (Nil, a)
      case FunType(args, ret) if ! args.isEmpty =>
         val argsE = args.map {case (_,t) => typeEras(t)._2}
         val retE = typeEras(ret)._2
         (argsE, retE)
      case _ => throw IllFormed
   }
   
   private def typeToScala(t: Term) : String = t match {
      case OMS(a) => nameToScalaQ(a) + ".univ"
      case ApplySpine(OMS(a), _) => typeToScala(OMS(a))
      case FunType(args, ret) if ! args.isEmpty =>
         val argsS = args.map(a => typeToScala(a._2))
         val retS = typeToScala(ret)
         argsS.mkString("(", " => ", " => ") + retS + ")"
      case _ => throw IllFormed
   }
      
   def exportCoveredTheory(t: DeclaredTheory) {
      outputHeader(t.parent.doc)
      outputTrait(t){c =>
         c.tp match {
            case None => ""
            case Some(tp) => try {
               val synName = nameToScala(t.name) + "." + nameToScala(c.name) + ".path"
               val semName = nameToScalaQ(c.path)
               val Some((args,ret)) = FunType.unapply(tp)
               val (decl, ini) = if (ret == Univ(1)) {
                  //scalaType(c.path)
                  val ini = s"  realizes {universe($synName)($semName)}"
                  val decl = c.df match {
                     case None =>
                        scalaVal(c.path, "RealizedType")
                     case Some(d) => d match {
                        // nice idea but does not work well; better expand all definitions if they are used later
                        // case OMS(p) => scalaValDef(c.path, Some("RealizedType"), nameToScalaQ(p))
                        case _ =>
                           scalaVal(c.path, "RealizedType")
                     }
                  }
                  (decl, ini)
               } else {
                  // create "realizes {function(name, argType1, ..., argTypeN, retType)(function)}
                  val (argsE, retE) = typeEras(tp)
                  val lts = (argsE ::: List(retE)).map(nameToScalaQ).mkString(", ")
                  val ini = s"  realizes {function($synName, $lts)($semName)}"
                  // create def name(x0: argType1._univ, ..., xN: argTypeN._univ): retType.univ
                  val names = args.zipWithIndex.map {
                     case ((Some(n), _), _) => n.toPath
                     case ((None   , _), i) => "x" + i.toString
                  }
                  val argsES = argsE.map(a => nameToScalaQ(a) + ".univ")
                  val decl = scalaDef(c.path, names zip argsES, nameToScalaQ(retE) + ".univ")
                  (decl, ini)
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
               decl +"\n" + ini + "\n"
            } catch {case IllFormed => "// skipping ill-formed " + c.name} 
         }
      }
      outputCompanionObject(t){c => ""}
   }
   
   def exportFunctor(v: DeclaredView) {}

   def exportRealization(v: DeclaredView) {}
}

import checking._
import uom._

/** this can be mixed into Scala-models of MMT theories to simplify adding additional rules */
trait SolutionRules extends RealizationInScala {
   def solve_unary(op:GlobalName, argType: RealizedType, rType: RealizedType)(invert: rType.univ => Option[argType.univ]) = {
      val sr = new SolutionRule(op / "invert") {
         def applicable(tm1: Term) = tm1 match {
            case ApplySpine(OMS(`op`), List(_)) => Some(1)
            case _ => None
         }
         def apply(j: Equality) = (j.tm1, j.tm2) match {
            case (ApplySpine(_, List(t)), rType(y)) => invert(y) map {x =>
                  (Equality(j.stack, t, argType(x), None), "inverting " + op.toString)
            }
            case _ => None
         }
      }
      rule(sr)
   }
   def solve_binary_right(op:GlobalName, argType1: RealizedType, argType2: RealizedType, rType: RealizedType)
            (invert: (rType.univ,argType2.univ) => Option[argType1.univ]) = {
      val sr = new SolutionRule(op / "right-invert") {
         def applicable(tm1: Term) = tm1 match {
            case ApplySpine(OMS(`op`), List(_,argType2(_))) => Some(1)
            case _ => None
         }
         def apply(j: Equality) = (j.tm1, j.tm2) match {
            case (ApplySpine(_, List(t, argType2(x2))), rType(y)) => invert(y,x2) map {x1 =>
                  (Equality(j.stack, t, argType1(x1), None), "inverting " + op.toString)
            }
            case _ => None
         }
      }
      rule(sr)
   }
   def solve_binary_left(op:GlobalName, argType1: RealizedType, argType2: RealizedType, rType: RealizedType)
            (invert: (argType1.univ,rType.univ) => Option[argType2.univ]) = {
      val sr = new SolutionRule(op / "left-invert") {
         def applicable(tm1: Term) = tm1 match {
            case ApplySpine(OMS(`op`), List(argType1(_),_)) => Some(2)
            case _ => None
         }
         def apply(j: Equality) = (j.tm1, j.tm2) match {
            case (ApplySpine(_, List(argType1(x1), t)), rType(y)) => invert(x1,y) map {x2 =>
                  (Equality(j.stack, t, argType2(x2), None), "inverting " + op.toString)
            }
            case _ => None
         }
      }
      rule(sr)
   }
}