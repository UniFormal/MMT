package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.{ComplexStep, ContentPath, DPath, GlobalName, LocalName, MPath, Path, SimpleStep}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.OMID
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.URI

class ViewCode(val  from :String, val to:String,val mappings: Map[String,String]){}

class ViewToMMT(ctrl: Controller) {
  var viewCount = 1

  def addView(view:ViewCode): Option[String] = {
    val from = ctrl.getTheory(Path.parse(view.from).asInstanceOf[MPath]).toTerm
    val to = ctrl.getTheory(Path.parse(view.to).asInstanceOf[MPath]).toTerm
    val mmtview = View.apply(DPath(URI.http colon("BenniDoes.Stuff")),LocalName.parse("GeneratedView_"+viewCount),from,to,false)
    ctrl.add(mmtview)
    view.mappings.map(mapping  =>{
      val name = LocalName(List(ComplexStep(Path.parse(view.from).asInstanceOf[MPath]),SimpleStep(mapping._1) ))
      val toCPath =  Path.parse(mapping._2).asInstanceOf[ContentPath]
      val df =  ctrl.get(toCPath).asInstanceOf[Constant].toTerm
      Constant.apply(mmtview.toTerm,name,List(),None,Some(df),None)
    }).foreach(c => ctrl.add(c))
    try{
      new ViewValidator(ctrl).checkView(mmtview)
      viewCount = viewCount +1
      Some("{ \"view\": \""+mmtview.path + "\" }")
    }catch{
      case e:Exception => {
        e.printStackTrace()
        ctrl.delete(mmtview.path)
        None
      }
    }

  }
}
