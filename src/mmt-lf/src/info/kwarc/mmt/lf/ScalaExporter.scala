package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._
import uom._

import GenericScalaExporter._

//TODO translate LF definitions to Scala definitions

/** for LF terms using Apply and Lambda */
class LFOperator(p: ContentPath, vars: ArgumentList, args: ArgumentList) extends Operator(vars,args) {
  private def omid = "OMID(this.path)"
  def makeTerm(pattern: Boolean) = {
     val argsS = if (pattern) args.namesConsed else args.nameList
     if (vars.args.isEmpty && args.args.isEmpty)
        omid
     else if (vars.args.isEmpty)
      s"ApplyGeneral($omid, $argsS)"
    else
      s"Apply($omid, Lambda(Context(${vars.nameList}), ${args.args.head.name}))"
  }
}

class ScalaExporter extends GenericScalaExporter {
   override val key = "lf-scala"
   override val packageSep = List("lf")

   override def outputHeader(dp: DPath) {
     super.outputHeader(dp)
     rh.writeln("import info.kwarc.mmt.lf._")
   }

   private object IllFormed extends Throwable
   private def typeEras(t: Term): (List[GlobalName],GlobalName) = t match {
      case OMS(a) => (Nil, a)
      case ApplySpine(OMS(a),_) => (Nil, a)
      case FunType(args, ret) if args.nonEmpty =>
         val argsE = args.map {case (_,t) => typeEras(t)._2}
         val retE = typeEras(ret)._2
         (argsE, retE)
      case _ => throw IllFormed
   }

   private def typeToScala(t: Term) : String = t match {
      case Univ(1) => "SemanticType.AllTypes"
      case OMS(a) => nameToScalaQ(a) + ".univ"
      case ApplySpine(OMS(a), _) => typeToScala(OMS(a))
      case FunType(args, ret) if args.nonEmpty =>
         val argsS = args.map(a => typeToScala(a._2))
         val retS = typeToScala(ret)
         argsS.mkString("(", " => ", " => ") + retS + ")"
      case _ => throw IllFormed
   }

   override def outputTrait(t: Theory) {
      return // switched off for now - causes more trouble than it's worth
      val includes = t.getIncludesWithoutMeta.filter {i =>
         controller.globalLookup.getO(i) match {
            case Some(r: RealizationInScala) =>
               false //TODO handle includes of models
            case Some(t: Theory) =>
               t.name.length == 1 // we only take basic theories for now
            case _ => false // should not happen
         }
      }
      val metas = objects.TheoryExp.metas(t.toTerm)(controller.globalLookup)
      val lfpos = metas.indexOf(LF.theoryPath)
      if (lfpos == -1) {
         log("skipping " + t.path + " (LF not meta-theory)")
         return
      }
      val metasUpToLF = metas.take(lfpos)
      val includesS = (metasUpToLF ::: includes).map(i => " with " + mpathToScala(i, packageSep)).mkString("")
      val tpathS = t.path.toString
      val name = nameToScala(t.name)
      rh.writeln(s"/** The type of realizations of the theory $tpathS */")
      rh.writeln(s"trait $name extends info.kwarc.mmt.lf.LFRealizationInScala$includesS {")
      val domainOver = if (includesS.isEmpty) "" else "override " // override included values
      rh.writeln(s"  ${domainOver}val _domain: TheoryScala = $name\n")
      t.getPrimitiveDeclarations foreach {
         case c: Constant if c.path.name.toPath != "int" =>
            val d = constantToString(t, c)
            rh.writeln(d)
         //TODO exclude declarations with extraneous types that should not be implemented, e.g., m:MOR a b
         case SimpleStructure(s, fromPath) if !s.isInclude =>
            // unnamed structures have been handled above already
            rh.writeln("  val " + nameToScalaQ(s.path) + ": " + mpathToScala(fromPath, packageSep))
         case _ =>
      }
      rh.writeln("}\n")
   }

    private def constantToString(t: Theory, c: Constant): String = {
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
                     scalaVal(c.path, "SemanticType")
                  case Some(d) => d match {
                     // nice idea but does not work well; better expand all definitions if they are used later
                     // case OMS(p) => scalaValDef(c.path, Some("RealizedType"), nameToScalaQ(p))
                     case _ =>
                        scalaVal(c.path, "SemanticType")
                  }
               }
               (decl, ini)
            } else {
               // create "realizes {function(name, List(argType1, ..., argTypeN), retType)(function)}
               val (argsE, retE) = typeEras(tp)
               val lts = argsE.map(nameInScala).mkString("List(", ", ", ")") + ", " + nameInScala(retE)
               val ini = s"  realizes {function($synName, $lts)($semName _)}"
               // create def name(x0: argType1.univ, ..., xN: argTypeN.univ): retType.univ
               val names = args.zipWithIndex.map {
                  case ((Some(n), _), _) => n.toPath
                  case ((None   , _), i) => "x" + i.toString
               }
               val argsES = argsE.map(a => "Any") //nameToScalaQ(a) + ".univ")
               val decl = scalaDef(c.path, names zip argsES, "Any") //nameToScalaQ(retE) + ".univ")
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
         } catch {case IllFormed =>
           "// skipping ill-formed " + c.name
         }
      }
   }

   override protected def companionObjectFields(c: Constant): List[String] = {
     lazy val default = super.companionObjectFields(c)
     // compute number of arguments
     val tp = c.tp getOrElse {return default}
     val tp2 = parser.ParseResult.fromTerm(tp).term
     val FunType(lfArgs, _) = tp2
     val numArgs = lfArgs.length
     // x1:Term,...,xn:Term
     val scalaArgs = Range(0,numArgs).toList map {i => Argument("x" + i.toString, "Term", sequence = false)}
     val op = new LFOperator(c.path, ArgumentList(Nil), ArgumentList(scalaArgs)) // TODO try to extract context
     op.methods
   }
}

import checking._

/** this can be used for Scala-native MMT theories to simplify adding additional rules */
abstract class LFRealizationInScala extends RealizationInScala {
  val under = List(Apply.path)
  def solve_unary(op:GlobalName, argTypeN: GlobalName, rTypeN: GlobalName)(invert: Any => Option[Any]) {
      val List(argType, rType) = List(argTypeN, rTypeN) map getRealizedType
      val sr = new ValueSolutionRule(op / "invert") {
         def applicable(tm1: Term): Option[Int] = tm1 match {
            case ApplySpine(OMS(`op`), List(_)) => Some(1)
            case _ => None
         }
         def apply(j: Equality): Option[(Equality, String)] = (j.tm1, j.tm2) match {
            case (ApplySpine(_, List(t)), rType(y)) => invert(y) map {x =>
                  (Equality(j.stack, t, argType of x, None), "inverting " + op.toString)
            }
            case _ => None
         }
      }
      rule(sr)
   }
   def solve_binary_right(op:GlobalName, argType1N: GlobalName, argType2N: GlobalName, rTypeN: GlobalName)
            (invert: (Any,Any) => Option[Any]) {
      val List(argType1, argType2, rType) = List(argType1N, argType2N, rTypeN) map getRealizedType
      val sr = new ValueSolutionRule(op / "right-invert") {
         def applicable(tm1: Term): Option[Int] = tm1 match {
            case ApplySpine(OMS(`op`), List(_,argType2(_))) => Some(1)
            case _ => None
         }
         def apply(j: Equality): Option[(Equality, String)] = (j.tm1, j.tm2) match {
            case (ApplySpine(_, List(t, argType2(x2))), rType(y)) => invert(y,x2) map {x1 =>
                  (Equality(j.stack, t, argType1 of x1, None), "inverting " + op.toString)
            }
            case _ => None
         }
      }
      rule(sr)
   }
   def solve_binary_left(op:GlobalName, argType1N: GlobalName, argType2N: GlobalName, rTypeN: GlobalName)
            (invert: (Any,Any) => Option[Any]) {
      val List(argType1, argType2, rType) = List(argType1N, argType2N, rTypeN) map getRealizedType
      val sr = new ValueSolutionRule(op / "left-invert") {
         def applicable(tm1: Term): Option[Int] = tm1 match {
            case ApplySpine(OMS(`op`), List(argType1(_),_)) => Some(2)
            case _ => None
         }
         def apply(j: Equality): Option[(Equality, String)] = (j.tm1, j.tm2) match {
            case (ApplySpine(_, List(argType1(x1), t)), rType(y)) => invert(x1,y) map {x2 =>
                  (Equality(j.stack, t, argType2 of x2, None), "inverting " + op.toString)
            }
            case _ => None
         }
      }
      rule(sr)
   }
}
