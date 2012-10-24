package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._
import modules._
import presentation._

 /**
  * A ConstantAssignment represents an MMT assignment to a constant.<p>
  * 
  * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent link
  * @param name the name of the instantiated symbol
  * @param target the term assigned to the symbol 
  */                                            // TODO term
class ConstantAssignment(val home : Term, val name : LocalName, val target : Term) extends Assignment {
   def toNode = <constant name={name.toPath}>{getMetaDataNode}{target.toOBJNode}</constant>
   override def toString = name + " |-> " + target.toString 
   def components = List(StringLiteral(name.toPath), target)
   def role = info.kwarc.mmt.api.Role_ConAss
}

  /**
   * A DefLinkAssignment represents an MMT assignment to a definitional link.<p>
   * 
   * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent link
   * @param name the name of the instantiated symbol
   * @param target the morphism assigned to the symbol 
   */
class DefLinkAssignment(val home : Term, val name : LocalName, val from: Term, val target : Term) extends Assignment {
   override def implicitKey = Some(from)
   def toNode = from match {
      case OMMOD(p) => <import name={if (name.isAnonymous) null else name.toPath} domain={p.toPath}>
                           {getMetaDataNode}
                           <value>{target.toOBJNode}</value>
                        </import>
      case _ =>         <import name={if (name.isAnonymous) null else name.toPath}>
                           {getMetaDataNode}
                           <domain>{from.toOBJNode}</domain>
                           <value>{target.toOBJNode}</value>
                        </import>
   }
   override def toString = (if (name.isAnonymous) "import " else name + " |-> ") + target.toString 
   def components = List(StringLiteral(name.toPath), target)
   def role = info.kwarc.mmt.api.Role_StrAss
}

class Open(val home : Term, val name : LocalName, val as : Option[String]) extends Assignment {
   def toNode = <open name={name.toPath} as={as.getOrElse(null)}>{getMetaDataNode}</open>
   override def toString = "open " + name.toPath + as.map(" " + _).getOrElse("")
   def components = List(StringLiteral(name.toPath), as.map(StringLiteral(_)).getOrElse(Omitted))
   def role = info.kwarc.mmt.api.Role_Open
}

/*
class Hides(parent : MPath, name : LocalPath) extends Assignment(parent, name) {
   def toNode = <hides name={name.flat}>{getMetaDataNode}</hides>
   def components = List(Text(name.flat))
   def role = "hiding"
}

*/