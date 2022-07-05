package info.kwarc.mmt.lf.itp

import info.kwarc.mmt.api.{ErrorHandler, ErrorThrower}
import info.kwarc.mmt.api.documents.InterpretationInstructionContext
import info.kwarc.mmt.api.objects.{Context, PlainSubstitutionApplier, Stack, Term, Typing, VarDecl}
import info.kwarc.mmt.api.parser.{NotationBasedParser, ParseResult, ParsingUnit, SourceRef}
import info.kwarc.mmt.api.proving.itp.Goal

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharArrayReader

/**
  * almost the same as [[TacticsParser]] but with the slight modification so that it can be extended without specifying the proof manager [[InteractiveProof]]
  * this is needed for the loadable tactics since they need to specify a parser whithout independently of a concrete [[InteractiveProof]]
  */
trait TacticsParserA extends JavaTokenParsers{
  val hypName: Regex = "([a-z]|[A-Z])+([a-z]|[A-Z]|[0-9]|[/_])*".r
  val hypNameWithSlash : Regex = "([a-z]|[A-Z]|/)+([a-z]|[A-Z]|[0-9]|[/_])*".r
  val anyExpr : Regex  = ".*".r
  val anyExprBrack : Regex  = "\\(.*\\)".r
  val anyExprBrackNoKeyWord : Parser[String]  = anyExprBrack >> {s => if (s.contains("in")) {failure("should not contain keyword \"in\"")} else {success(s)} }
  val spaces : Regex = """[ \n\t\s]*""".r
  val nespaces : Regex = """[ \n\t\s]+""".r

  //  override val whiteSpace : Regex = """\s+""".r
  override val skipWhitespace  = false

  // override def handleWhiteSpace(source: CharSequence, offset: Int): Int = super.handleWhiteSpace(source, offset)


  def simpleTermParser(t : String, ip : InteractiveProof) : ( Term, Context) = {

    val c = ip.slvr.controller
    val nbp = c.extman.get(classOf[NotationBasedParser]).head
    val cxt = ip.pr.currentState.head.ctx  ++ (if (ip.mp.isDefined) Context(ip.mp.get) else Context())
    val pu = ParsingUnit(SourceRef.anonymous("interactive") , cxt , t ,InterpretationInstructionContext(ip.slvr.controller.getNamespaceMap) )
    val eh : ErrorHandler =  ErrorThrower
    val ParseResult(u,f,tt) = nbp(pu)(eh)
    //    val newmiss = u.map(x => ip.pr.getCurrentGoal.makeMiss(x.name))
    (tt, u )
  }


  def currGoalParseTerm(t : String , ip : InteractiveProof) : ( Term, Context) = {

    val c = ip.slvr.controller
    val nbp = c.extman.get(classOf[NotationBasedParser]).head
    val cxt = ip.pr.currentState.head.ctx  ++ (if (ip.mp.isDefined) Context(ip.mp.get) else Context())
    val pu = ParsingUnit(SourceRef.anonymous("interactive") , cxt , t ,InterpretationInstructionContext(ip.slvr.controller.getNamespaceMap) )
    val eh : ErrorHandler =  ErrorThrower
    val ParseResult(u,f,tt) = nbp(pu)(eh)
    val Goal(g , gctx  , ukn) = ip.pr.getCurrentGoal
    //    val newmiss = u.map(x => ip.pr.getCurrentGoal.makeMiss(x.name))
    ip.pr.update(Goal(g , gctx  , ukn ))
    ip.slvr.addUnknowns(u, None)

    (tt, u )
  }



  def currGoalParseTermA(t : String, ip : InteractiveProof) : ( Term, Context) = {


    def newnames(ctx0 : Context, tt : Term): (Context, Term) = {
      val ps = ip.slvr.getPartialSolution
      var res = Context()
      var restt = tt

      def loop(ctx : List[VarDecl]) : Unit = ctx match {
        case Nil =>
        case (x::xs) => {
          val (newn , subn) = Context.pickFresh(Context(xs : _ *) ++ res ++ ps , x.name)
          restt = restt.substitute(subn)(PlainSubstitutionApplier)
          res = res ++ x.copy(name = newn)
          loop(xs)
        }
      }
      loop(ctx0.variables.toList)
      (res , restt)
    }

    val c = ip.slvr.controller
    val nbp = c.extman.get(classOf[NotationBasedParser]).head
    val cxt = ip.pr.currentState.head.ctx  ++ (if (ip.mp.isDefined) Context(ip.mp.get) else Context())
    //   val uxt = ip.pr.getCurrUks.map(x => x.ln)
    val pu = ParsingUnit(SourceRef.anonymous("interactive") , cxt  , t ,InterpretationInstructionContext(ip.slvr.controller.getNamespaceMap) )
    val eh : ErrorHandler =  ErrorThrower
    val ParseResult(u0,frees,tt0) = nbp(pu)(eh)
    val gl@Goal(g , gctx , ukn) = ip.pr.getCurrentGoal

    val (u,tt1) = newnames(u0,tt0)
    lazy val sub = u.map(x => new ProofUtil.ExtendUnkownInTerm( x.name , gctx )) // Substitution(u.variables.map(v => Sub(v.name , gl.makeUnknown(ip.slvr , v.name))) : _*)
    val tt = if (gctx.isEmpty) tt1 else sub.foldLeft(tt1)((tres , sub) => sub(tres , gctx)) // tt1.substitute(sub)(PlainSubstitutionApplier)
    ip.slvr.addUnknowns(u , None)
    val tmp =  ip.slvr.inferType(tt , false)(Stack(ip.pr.getCurrCtx), ip.hist)    // ip.slvr.asInstanceOf[ExtendedSolver].standAloneInfer(tt,u,ip.pr.getCurrCtx,ip.hist) // ip.slvr.inferType(tt , false)(Stack(ip.pr.getCurrCtx), ip.hist)  //check(Typing(Stack(Context()) , tm ,  tp))(new History(Nil))
    val tmp1 = ip.slvr.getPartialSolution.filter(x => u.index(x.name).isDefined)
    val ukres = tmp match {
      case None => throw new TacticError("could not infer type of: " + ip.slvr.presentObj(tt))
      case Some(ttp) => {
        val (res, ures ) = ProofUtil.standAloneTypeCheck(tt , ttp , tmp1 , ip.pr.getCurrCtx, ip.slvr)
        Context(ures.map(x => if (x.df.isDefined && ProofUtil.containsMissing(x.df.get))   x.copy(df = None) else x ) : _ *)
      }
    }

    /*   val alluks =  ukres // ip.slvr.getPartialSolution
       alluks.foreach(x => (x.tp , x.df) match {
         case (None , None) => throw new TacticError("could not infer type of unkown " + x.name.toString + " in term " + ip.slvr.presentObj(tt) + " (raw: " + tt.toString + " )")
         case (Some(tpv) , None) =>
         case (None , Some(dfv)) => {
         }
         case _ =>
       })

     */
    // ip.slvr.addUnknowns(ukres, if (u.nonEmpty) Some(u(0).name) else None)
    // var ttres = tt
    ukres.foreach(x => {
      if (x.df.isDefined && ! ProofUtil.containsMissing(x.df.get)) {
        ip.slvr.solve(x.name , x.df.get)(ip.hist)
        //   val newsub = Substitution(Sub(x.name , x.df.get))
        //   ttres = ttres ^ newsub
      }
      if (x.tp.isDefined && ! ProofUtil.containsMissing(x.tp.get)) ip.slvr.solveTyping(Typing(Stack(gctx) , x.toTerm , x.tp.get ))(ip.hist)
    })

    // val us = ip.slvr.getUnsolvedVariables
    val newuks = ukres.filter(x => x.df.isEmpty) // us.variables.filter(p => u.variables.exists((x : VarDecl) => x.name == p.name))
    val newukgoals = newuks.map(v => Goal(ProofUtil.remOuterFree(v.tp.get , gctx) , ip.pr.getCurrCtx, v.name))
    ip.pr.currentState.appendAll(newukgoals )
    val ukret = ukres.map(v => {
      val newtp = v.tp match {
        case None => None
        case Some(tpp) => if (ProofUtil.containsMissing(tpp)) None else Some(tpp)
      }
      val newdf = v.df match {
        case None => None
        case Some(dff) => if (ProofUtil.containsMissing(dff)) None else Some(dff)
      }
      v.copy(tp = newtp , df = newdf)
    })
    //  val subs  = Substitution( ukre : _ *)
    //   val ttnew =  tt & subs
    val ttres = ip.slvr.substituteSolution(tt)
    (ttres ,ukret)
  }


  abstract class EParser[+T] extends Parser[T] {
    def ?~>[A](p : => Parser[A]) : Parser[Option[A]] = {
      this ~> """\s*""".r ~ p.? >> {
        case ws ~ (r@Some(pr)) => if (ws.length > 0) success(r) else failure("arguments have to be separated by whitespace")
        case ws ~ None => success(None)
      }
    }

    def ??~>[A](p : => Parser[Option[A]]) : Parser[Option[A]] = {
      this ~> """\s*""".r ~ p >> {
        case ws ~ (r@Some(pr)) => if (ws.length > 0) success(r) else failure("arguments have to be separated by whitespace")
        case ws ~ None => success(None)
      }
    }

    def &~>[A](p : => Parser[A]) : Parser[A] = {
      this ~> """\s+""".r ~> p

    }

    def &~[A](p : => Parser[A]) : Parser[~[T,A]] = {
      this ~ """\s+""".r ~ p ^^ {case a ~ b ~ c => new ~(a , c) }

    }

    def ??~[A](p : => Parser[Option[A]]) : Parser[~[T,Option[A]]] = {
      this ~ """\s*""".r ~ p >> {
        case a ~ ws ~ (r@Some(pr)) => if (ws.length > 0) success(new ~(a,r)) else failure("arguments have to be separated by whitespace")
        case a~ ws ~ None => success(new ~(a,None))
      }
    }

    def ?~[A](p : => Parser[A]) : Parser[~[T,Option[A]]] = {
      this ~ """\s*""".r ~ p.? >> {
        case a ~ ws ~ (r@Some(pr)) => if (ws.length > 0) success(new ~(a,Some(pr))) else failure("arguments have to be separated by whitespace")
        case a~ ws ~ None => success(new ~(a,None))
      }
    }

    def done[A](p : => Parser[A]) : Parser[A] = {
      """\s*""".r ~> p <~ """\s*""".r
    }


  }

  implicit def ptoep[A](p : Parser[A]) : EParser[A] = {
    new EParser[A]{override def apply(i : Input) = p.apply(i)}
  }

  implicit def eliteral(s : String): EParser[String] = {
    ptoep[String](literal(s))
  }

  implicit def eregex(r : Regex) : EParser[String] = {
    ptoep(regex(r))
  }

  def inbalbracksOpt[A](p : => Parser[A]) : Parser[A] = {
    new EParser[A]{
      override def apply(i : Input): ParseResult[A] = {
        if(i.atEnd) return Failure("empty expression", i)
        var curr : Char = i.first
        var rest = i.rest
        var oldrest = rest
        while((! i.atEnd) && List(' ' , '\t' , '\n' ).contains(curr)  ){
          curr = rest.first
          oldrest = rest
          rest = rest.rest
        }
        if(curr == '(') {
          var bcount = 1
          val newi : ArrayBuffer[Char] = ArrayBuffer[Char](curr)
          while ((!rest.atEnd) && bcount != 0){
            curr = rest.first
            oldrest = rest
            rest = rest.rest
            if (curr == '(') {
              bcount += 1
            }else if (curr == ')'){
              bcount -= 1
            }
            newi.append(curr)
          }
          val newi0  = new CharArrayReader(newi.toArray,0)
          if (rest.atEnd) oldrest  = rest
          p.apply(newi0) match {
            case Success(result, next) => {
              if (! next.atEnd) return Failure("could not parse argument", next)
              else {
                Success(result , rest)
              }
            }
            case ns : NoSuccess => ns
          }
        }else {
          val newi : ArrayBuffer[Char] = ArrayBuffer[Char](curr)
          while((! rest.atEnd) && ! List(' ' , '\t' , '\n' ).contains(curr)){
            curr = rest.first
            oldrest = rest
            rest = rest.rest
            if (! List(' ' , '\t' , '\n' ).contains(curr))  newi.append(curr)
          }
          if (rest.atEnd) oldrest  = rest
          val newi0  = new CharArrayReader(newi.toArray,0)
          p.apply(newi0) match {
            case Success(result, next) => {
              if (! next.atEnd) return Failure("could not parse argument", next)
              else {
                Success(result , oldrest)
              }
            }
            case ns : NoSuccess => ns
          }
        }
      }
    }
  }
}