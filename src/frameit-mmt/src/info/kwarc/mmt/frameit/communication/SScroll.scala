package info.kwarc.mmt.frameit.communication

import info.kwarc.mmt.frameit.business.Scroll
import info.kwarc.mmt.frameit.communication.SimpleOMDoc.SDeclaration

case class SScroll(problemTheory: SimpleOMDoc.SURI, solutionTheory: SimpleOMDoc.SURI, label: String, description: String, declarations: List[SDeclaration])

object SScroll {
  def fromScroll(scroll: Scroll): SScroll = SScroll(
    scroll.problemTheory.toString,
    scroll.solutionTheory.toString,
    scroll.label,
    scroll.description,
    scroll.declarations.map(SimpleOMDoc.OMDocBridge.encode)
  )
}

case class SScrollApplication(scroll: SScroll, view: String)

/*
val metaTags = getMetaTags()
  val isScrollKey = metaTags.find( _.name.toString().contains("scrollpart")).get
  val isProofKey = metaTags.find( _.name.toString().contains("parameterType")).get
  val factValKey = metaTags.find(_.name.toString().contains("factValue")).get
  val solTheoryKey = metaTags.find( _.name.toString.contains("solutionTheoryURI")).get
  val scrollnameKey = metaTags.find( _.name.toString.contains("scrollname")).get
  val scrollDescriptionKey = metaTags.find( _.name.toString.contains("scrollDescriptionUI")).get
  val paraDescriptionKey = metaTags.find( _.name.toString.contains("parameterDescriptionUI")).get

  def getMetaTags(): List[Declaration] = {
    val metaTheory = ctrl.depstore.getInds(IsTheory)
      .find(p => p.toString().endsWith("ScrollMeta"))
        .getOrElse(throw  new IllegalStateException(" could not find ScrollMetas"))
    ctrl.getTheory( metaTheory.asInstanceOf[MPath]).getDeclarations
  }


  def isScroll( t: Theory) : Boolean = {
    val  ret = t.metadata.keys.map(_.name).contains(isScrollKey.name)
    ret
  }

  def convertToJson(l: Iterator[Theory]): String ={
    val tmp = l.toList
    if (tmp.isEmpty) "{}" else
    "{\"Scrolls\": [" + tmp.map( convertToJson(_)).foldLeft("")((a,b) => a +","+ b).tail+ "]}"
  }

  def convertToJson( t: Theory) : String = {
    val problemTheory = t.path.toString()
    val solTheory = t.metadata.get(solTheoryKey.path).head.value.toString()
    val name = t.metadata.get(scrollnameKey.path).head.value.toString()
    val description = t.metadata.get(scrollDescriptionKey.path).head.value.toString()
    val declarations = t.getDeclarations.
      filter( d =>   d.metadata.keys.nonEmpty && d.metadata.get(paraDescriptionKey.path).nonEmpty).
      map( d => {
      val name = d.name
      val isProof = d.metadata.keys.contains(isProofKey.path)
      val value = d.metadata.get(factValKey.path)
      val uri = d.path
      val descriptionUI = d.metadata.get(paraDescriptionKey.path).head.value
      "{" +
        " \"name\": \"" + name + "\","+
        " \"isProof\": \""+ isProof + "\","+ {
          if (!value.isEmpty)
            " \"value\" : \"" + Path.parse(value.head.value.toString()).last + "\","
          else ""
        }+
        " \"identifier\": \"" + uri.name + "\","+
        "\"description\": \"" + descriptionUI + "\"" +
      "}"
    })
    "{ \"problemTheory\":\"" + problemTheory + "\","+
      " \"solutionTheory\": \""+ solTheory + "\","+
      "\"label\":\""+ name +"\", "+
      "\"description\":\"" + description + "\","+
      "\"declarations\":"+"[" + declarations.foldLeft("")((a,b) => a+","+b).tail+ "]" +
     "}"
  }
 */
