package mizar.objects

import mizar.objects._
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
	var vocabulary : List[String] = Nil
	val dictionary : Dictionary = new Dictionary(Nil)
	var currentDefBlock : Option[XMLDefinitionBlock] = None
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
	
	def setDefBlock(db : XMLDefinitionBlock) : Unit = {
		currentDefBlock = Some(db)
	}
	
	
	def addIsDefiniens(d : XMLIsDefiniens) : Unit = {
		currentDefBlock match {
			case Some(db) => {
				db.defs.find(x => x.relnr == d.constrnr) match {
					case Some(df) => {
						currentArticle match {
							case Some(art) => {
								df.kind match {
									case "M" =>
										//val name = dictionary.getNameByFormatnr(df.kind, df.relnr) //TODO
										val opArgs = df.resolveArgs()
										opArgs match {
											case Some(args) =>
												val m = new MizModeIsDef(resolveDef(df.symbolnr, df.kind), df.aid, df.relnr, args, df.retType, d.term)
												art.addElem(m)
											case None => "Err in Controller -> addIsDefiniens : unable to parse Mode arg names"
										}
									case "K" | "O" =>
										df.retType match {
											case Some(rt) => 
												val opArgs = df.resolveArgs()
												opArgs match {
												case Some(args) =>
													val f = new MizFuncIsDef(resolveDef(df.symbolnr, df.kind), df.aid, df.relnr, args, rt, d.term)
													art.addElem(f)
												case None => "Err in Controller -> addIsDefiniens : unable to parse Func arg names"
											}												
											case None => println("err in Controller -> addIsDefiniens : Func with no return type")												
										}
									case "R" => 
										val opArgs = df.resolveArgs()
										opArgs match {
											case Some(args) =>
												val p = new MizPredIsDef(resolveDef(df.symbolnr, df.kind), df.aid, df.relnr, args, d.term)
												art.addElem(p)
											case None => "Err in Controller -> addIsDefiniens : unable to parse Mode arg names"
										}
										
								}
							}
							case None => println("err -> article not init for MizIsDefiniens: " + d.aid + d.constrkind + d.constrnr)
						}
					}
					case None => println("err -> no constructor found for: " + d.aid + d.constrkind + d.constrnr)
				}
			}
			case None => println("err -> no currentDefBlock for -> " + d.aid + d.constrkind + d.constrnr)
		}
	}
	
	def addMeansDefiniens(d : XMLMeansDefiniens) : Unit = {
		currentDefBlock match {
			case Some(db) => {
				db.defs.find(x => x.relnr == d.constrnr) match {
					case Some(df) => {
						currentArticle match {
							case Some(art) => {
								df.kind match {
									case "M" =>
										//val name = dictionary.getNameByFormatnr(df.kind, df.relnr) //TODO
										val opArgs = df.resolveArgs()
										opArgs match {
											case Some(args) =>
												val m = new MizModeMeansDef(resolveDef(df.symbolnr, df.kind), df.aid, df.relnr, args, df.retType, d.form)
												art.addElem(m)
											case None => "Err in Controller -> addMeansDefiniens : unable to parse Mode arg names"
										}
									case "K" | "O" =>
										df.retType match {
											case Some(rt) => 
												val opArgs = df.resolveArgs()
												opArgs match {
												case Some(args) =>
													val f = new MizFuncMeansDef(resolveDef(df.symbolnr, df.kind), df.aid, df.relnr, args, rt, d.form)
													art.addElem(f)
												case None => "Err in Controller -> addMeansDefiniens : unable to parse Func arg names"
											}												
											case None => println("err in Controller -> addMeansDefiniens : Func with no return type")												
										}
									case "R" => 
										val opArgs = df.resolveArgs()
										opArgs match {
											case Some(args) =>
												val p = new MizPredMeansDef(resolveDef(df.symbolnr, df.kind), df.aid, df.relnr, args, d.form)
												art.addElem(p)
											case None => "Err in Controller -> addMeansDefiniens : unable to parse Mode arg names"
										}
										
								}
							}
							case None => println("err -> article not init for MizMeanssDefiniens: " + d.aid + d.constrkind + d.constrnr)
						}
					}
					case None => println("err -> no constructor found for: " + d.aid + d.constrkind + d.constrnr)
				}
			}
			case None => println("err -> no currentDefBlock for -> " + d.aid + d.constrkind + d.constrnr)
		}
	}
	
	def resolveDef(symbolnr : Option[Int], kind : String) : Option[String] = {
		symbolnr match {
			case Some(snr) => dictionary.symbols .find(x => (x.symbolnr == snr) && (x.kind == kind) ) match {
				case Some(x) => Some(x.name)
				case None => None
			}
			case None => None
		}
	}
	
	def resolveDef(symbolnr : String, kind : String, absnr : Int) : Option[String] = {
		try {
			val snr = symbolnr.toInt
			dictionary.symbols.find(x => (x.symbolnr == snr) && (x.kind == kind) ) match {
				case Some(x) => Some(x.name)
				case None => 
					
					symbolnr + kind match {
					case "1M" => Some("set")
					case "1R" => Some("==") 
					case _ => println(symbolnr + kind)
					None
				}
			}
		} catch {
			case e : Exception => 
				kind + absnr match {
					case "M1" => Some("set")
					case "R1" => Some("==")
					case _ => None
				}
		}
	}

	
	def resolveVar(Svid : String) : Option[String] = {
		try {
			val vid = Svid.toInt
			dictionary.symbols.find(x => x.symbolnr == vid && (x.kind == "I")) match {
				case Some(x) => Some(x.name)
				case None => 
				None
			}
		} catch {
			case e : Exception => 
			None
		}
	}
	
}
