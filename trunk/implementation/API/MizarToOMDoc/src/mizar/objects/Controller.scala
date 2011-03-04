package mizar.objects
import scala.collection.immutable._

/**
 * Trait for all Mizar classes
 */
trait MizAny

/**
 * Class for Parser errors
 */
class MizErr() extends MizAny

object ParsingController {
	
	var currentDefBlock : Option[MizDefinitionBlock] = None
	var currentArticle : Option[MizArticle] = None
	
	def setArticle(aid : String) {
		currentArticle = Some(new MizArticle(aid, Nil))
	}
	
	def addToArticle(el : MizAny) : Unit = {
		currentArticle match {
			case Some(art) => art.addElem(el)
			case None => println("err, article not initialised")
		}
	}
	
	def setDefBlock(db : MizDefinitionBlock) : Unit = {
		currentDefBlock = Some(db)
	}
	
	def buildDefinition(d : MizDefTheorem) : Unit = {
		currentDefBlock match {
			case Some(db) => {
				db.defs.find(x => x.relnr == d.constrnr) match {
					case Some(df) => {
						currentArticle match {
							case Some(art) => {
								df.setProp(d.prop)
								art.addElem(df)
							}
							case None => println("err-> article not init")
						}
					}
					case None => println("err-> no constructor found for" + d.constrnr.toString + " " + d.constrkind)
						db.defs.map(x => print(x.relnr + " "))
						println(db)
				}
			}
			case None => println("err-> no currentDefBlock")
		}
	}
	
	
}