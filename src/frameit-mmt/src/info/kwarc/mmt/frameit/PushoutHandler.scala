package info.kwarc.mmt.frameit

import info.kwarc.mmt.frameit.mitm.Foundation.RealLiterals
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{OMA, OMID, OMS, Term}
import info.kwarc.mmt.api.symbols.{Declaration, FinalConstant}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{ApplySpine, Arrow}
import info.kwarc.mmt.moduleexpressions.operators.NamedPushoutUtils

class PushoutHandler (ctrl : Controller) {

  var counter = 0

  def generatePushout( problem: String, solution: String , situation: String, view: String):Theory = {
    val probPath = Path.parse(problem).asInstanceOf[MPath]
    val solPath = Path.parse(solution).asInstanceOf[MPath]
    val sitPath =  Path.parse(situation).asInstanceOf[MPath]
    val viewPath = Path.parse(view).asInstanceOf[MPath]

    val genViewPath = MPath(DPath(URI.http colon("BenniDoes.Stuff")), LocalName("pushView_"+counter))
    val genTheoPath = MPath(DPath(URI.http colon("BenniDoes.Stuff")), LocalName("pushTheo_"+counter))
    val outpush = NamedPushoutUtils.computeCanonicalPushoutAlongDirectInclusion(
      ctrl.getTheory(probPath),
      ctrl.getTheory(sitPath),
      ctrl.getTheory(solPath),
      genTheoPath,
      ctrl.getAs(classOf[View],viewPath),
      genViewPath
    )
    counter = counter +1
   outpush._1
  }

  def readPushout(theory:Theory) = {
    val facts : List[String] =  theory.getDeclarations.
      filter(_.isInstanceOf[FinalConstant]).
      map(_.asInstanceOf[FinalConstant]).
      map((const : FinalConstant) => {
        println(const.tp)
        const.tp match {
          case None => None
          case Some(RealLiterals.synType) =>  None

          case Some(Arrow(t1:Term, t2:Term)) =>
          // Proof Operator
            None
          case Some(OMS( t:GlobalName)) =>
            // Vector or Line
            t.toString() match{
              case f: String if(f.contains("vector")) => {
                const.df.get match {
                  case ApplySpine(f: OMID, args: List[Term]) => {
                    val argsDouble = args.map(_.toString().toDouble)
                    Some(new Vector(argsDouble(0), argsDouble(1), argsDouble(2)).toJSONString(const.path.toString()))
                  }
                }
              }
              case f: String if (f.contains("line")) => {
                const.df.get match{
                  case ApplySpine (func: OMID, args: List[Term]) =>{
                    val argsString = args.map(_.toString())
                    Some ( new Line(argsString(0), argsString(1)).toJSONString(const.path.toString()))
                  }
                }
              }
              case default => {
                println(t)
                None
              }
            }
          case Some(ApplySpine(typeFunc :Term, args: List[Term])) =>
            // Distance, Angle
            typeFunc.toString() match {
              case f:String  if(f.contains("distanceFact") ) => {
                val argsString = args.map(_.toString())
                Some(new DistanceFact(argsString(0),argsString(1),argsString(2)).toJSONString(const.path.toString()))
              }
              case f:String  if(f.contains("angleFact") ) => {
                val argsString = args.map(_.toString())
                Some(new AngleFact(argsString(0),argsString(1),argsString(2),argsString(3)).toJSONString(const.path.toString()))
              }
              case f:String if(f.contains("onLineFact")) => {
                val argsString = args.map(_.toString())
                Some( new OnLineFact(argsString(0),argsString(1)).toJSONString(const.path.toString()))
              }
              case default =>{
                None
              }
            }
          case default => None
        }
      }
    ).filter(_.isDefined).map(_.get)

    "\"outputs\": [" + facts.tail.foldLeft(facts.head)((a,b) => a+","+ b ) + "]"
  }

}
