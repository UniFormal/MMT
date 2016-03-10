package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{LocalName, ComplexStep, GlobalName, MPath}

/** Helper class for intersecter; basically wraps around FinalConstants, includes and structures */

sealed trait TheorySet {
  // all constants (recursively)
  val orig : List[FinalConstant]
  def allconsts: Set[(FinalConstant,List[GlobalName])] = localconsts ++ includes.flatMap(_.allconsts)

  var structures : List[Structure]
  // all constants locally declared (nonrecursively)
  var localconsts : Set[(FinalConstant,List[GlobalName])]
  // this can be given a path for the toView method
  private var path: Option[MPath] = None
  def setPath(p:MPath) {path = Some(p)}
  def getPath = path
  // direct theory inclusions
  var includes: List[TheorySet]
  private var metath : Option[MPath] = None
  def setMeta(p:MPath) {metath = Some(p); includes.foreach(_.setMeta(p))}
  def meta = metath
  // all theory inclusions recursively
  def allincludes: List[TheorySet] = (includes ::: includes.flatMap(_.allincludes)).distinct

  private def makesub : Set[(GlobalName,GlobalName)] = {
    require(path.isDefined)
    localconsts.flatMap(p => p._2.tail.map(n => (path.get ? p._2.head.name,n)))++includes.flatMap(_.makesub)
  }

  def toTheory(implicit controller: Controller) : List[DeclaredTheory] = toDeclaredTheory()._1

  private def toDeclaredTheory(sub:Set[(GlobalName,GlobalName)] = Set())(implicit controller:Controller) : (List[DeclaredTheory],Set[(GlobalName,GlobalName)]) = {
    require(path.isDefined)
    var subst = if (sub.isEmpty) makesub else sub

    val consts = localconsts map(c => if(c._1.name == c._2.head.name) c._1 else {
      val d = controller.get(c._2.head) match {
        case e: FinalConstant => e
        case _ => throw new Exception("Constant "+c._2.head+" doesn't exist!")
      }
      d
    })

    val th = new DeclaredTheory(path.get.doc,path.get.name,metath)
    structures.foreach(s => th.add(s))

    val losts = orig.filter(c => !localconsts.exists(p => p._1==c))

    val newincs = includes.foldLeft((List().asInstanceOf[List[DeclaredTheory]],subst)) ((nsub,inc) =>{
      var (incth,newsubst) = inc.toDeclaredTheory(nsub._2)
      val theselosts = losts.map(c => (c,inc.allconsts.find(d => d._2.contains(c.path)))).collect{
        case (c,Some((d,l))) =>
          val sourceth = (inc::inc.allincludes).find(ts => ts.localconsts.contains((d,l))).get.getPath.get
          (c,incth.collectFirst{case t if t.declares(l.head.name) => t.get(l.head.name) match {
            case e:FinalConstant => e
            case _ => throw new Exception("Constant expected")
          }}.getOrElse(throw new Exception("Constant went missing: "+l.head.name+" in "+incth)))
      }

      def getName(name:LocalName, i:Int = 0) : LocalName = {
        val tryname = if (i==0) name else LocalName(name.toString+"_"+i)
        if (th.declares(tryname)) getName(name,i+1) else tryname
      }

      if (!theselosts.exists(p => p._1.name!=p._2.name || p._1.df.isDefined)) th add PlainInclude(incth.head.path,th.path) else {
        val s = DeclaredStructure(th.toTerm,getName(incth.head.name),incth.head.toTerm,false)
        theselosts foreach (p => if(p._1.name!=p._2.name || p._1.df.isDefined) {
          val c = Constant(s.toTerm,ComplexStep(p._2.home.toMPath)/p._2.name,if(p._1.name!=p._2.name) List(p._1.name) else Nil,
          p._2.tp,p._1.df,None)
          s add c
          newsubst = newsubst.map(q => if(q._1==p._1.path) (c.path,q._2) else q)+((c.path,p._1.path))
        })
        th add s
      }
      (incth,newsubst)
    })
    Moduleadder(th,consts.toList,newincs._2.toList)
    controller.library.delete(th.path)
    controller.library.add(th)
    (th::newincs._1,newincs._2)
  }

  private def toStringIndent(ind:String) : String =
    ind+(if (path.isDefined) path.get.name.toString else "NoPath")+" : "+
      (if (metath.isDefined) metath.get.name.toString else "NoMeta")+
      includes.map(x => "\n"+x.toStringIndent(ind+"  ")).mkString("\n")+
      localconsts.map(p => "\n  "+ind+p._1.name+" : "+p._1.tp+" = "+p._1.df).mkString("")

  override def toString = toStringIndent("")
}

// Factory for TheorySets
object TheorySet {

  class TS(val consts:List[FinalConstant], val sub:List[TheorySet], val structs:List[Structure]) extends TheorySet {
    var localconsts = consts.map(c => (c,List(c.path))).toSet
    var includes = sub.distinct
    var structures = structs.distinct
    val orig = consts
  }
  var existing : Set[TS] = Set()

  def reset = existing = Set()

  def apply(th:DeclaredTheory,metaasincludes : Boolean)(implicit controller: Controller) : TheorySet = {
    val consts = th.getConstants collect {case c: FinalConstant => c}
    val subs = if(metaasincludes) th.getIncludes map controller.get collect {case t: DeclaredTheory => t} map(apply(_,metaasincludes))
    else th.getIncludesWithoutMeta map controller.get collect {case t: DeclaredTheory => t} map(apply(_,metaasincludes))
    val ts = TheorySet(consts,subs,th.getNamedStructures,Some(th.path))
    th.meta foreach ts.setMeta
    ts
  }


  def apply(consts:List[FinalConstant],sub:List[TheorySet] = Nil, structs : List[Structure] = Nil, path:Option[MPath] = None)
    : TheorySet = {
    require(consts.nonEmpty || sub.nonEmpty || structs.nonEmpty)
    existing.find(ts => (path.isDefined && ts.getPath.isDefined && path.get==ts.getPath.get)||(ts.consts==consts && ts.sub==sub & ts.structs==structs)).getOrElse({
      val ts = new TS(consts,sub,structs)
      if (path.isDefined) ts.setPath(path.get)
      existing+=ts
      ts
    })
  }

}