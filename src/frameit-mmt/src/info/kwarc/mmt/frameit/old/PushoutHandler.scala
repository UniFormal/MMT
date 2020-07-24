package info.kwarc.mmt.frameit.old

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, LocalName, MPath, Path}
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
    ???
  }

}
