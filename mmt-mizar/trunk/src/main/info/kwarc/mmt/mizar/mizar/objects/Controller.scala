package info.kwarc.mmt.mizar.mizar.objects

import scala.collection.mutable._
//import scala.collection.immutable._
/**
 * Trait for all Mizar classes
 */
trait MizAny

/**
 * Class for Parser errors
 */
class MizErr() extends MizAny

object ParsingController {
	//var vocabulary : List[String] = Nil
	val dictionary : Dictionary = new Dictionary(Nil)
	
	var selectors : Map[String,Map[Int,Tuple2[Int,Int]]] = new HashMap()
	val attributes : Map[String,Map[Int,Int]] = new HashMap()
	//var definitions : LinkedList[XMLDefinition] = new LinkedList()
	var elems : LinkedList[MizAny] = new LinkedList()
	var definiens : LinkedList[XMLDefiniens] = new LinkedList()
	//val deftheorems : List[MizDefTheorem]
	//var currentDefBlock : Option[XMLDefinitionBlock] = None
	//var currentArticle : Option[MizArticle] = None
	var currentAid = ""
	
		/*
	def setArticle(aid : String) {
		currentArticle = Some(new MizArticle(aid, Nil))
		currentAid = aid
	}
	*/
	def addToArticle(el : MizAny) : Unit = {
			elems = elems :+ el
	}

	
	
	def addDefiniens(d : XMLDefiniens)  = {
		definiens = definiens :+ d
	}
	
	def addDefinitionBlock(db : XMLDefinitionBlock) = {
		elems = elems ++ db.defs 
	}
	
	def buildArticle() : MizArticle = {
		val art = new MizArticle(currentAid, Nil)
		elems.map(x => {
			x match {
				case d : XMLDefinition =>
					buildDefinitions(d, art) 
				case _ =>
					art.addElem(x)
			
			}
		})
		
		elems = new LinkedList()
		definiens = new LinkedList()
		art
	}
	
	def buildDefinitions(d : XMLDefinition, art : MizArticle) :  Unit =  {
			val df = definiens.find(x => (d.nr == x.absconstrnr && d.kind == x.constrkind && d.aid == x.constraid))  match {
				case Some(dfn) => {
					dfn
				}

				case None => d match {
				case rd : XMLRedefinition => {
					new XMLIsDefiniens(d.aid, d.nr : Int, d.aid, d.kind, d.nr,Nil, Some(new MizFunc(rd.constraid, d.kind, rd.constrabsnr, rd.argTypes.zipWithIndex.map(x => new MizLocusVar(x._2 + 1)))))//TODO check
				}
					case _ => println("Err in Controller -> buildDefinitions : no definiens found for definition")
				} 
			}

			val opArgs = d.resolveArgs()
			opArgs match {
			case Some(args) => {
				d.kind match {
				case "R"  => {
					val p = df match { 
					case di  : XMLIsDefiniens => 								
					new MizPredIsDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, di.cases, di.term)
					case dm: XMLMeansDefiniens => 								
					new MizPredMeansDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, dm.cases, dm.form)
					}
					art.addElem(p)
				}
				case "K" | "O" => {
					d.retType match {
					case Some(rt) => {
						val f = df match { 
						case di  : XMLIsDefiniens => 								
						new MizFuncIsDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.kind, d.nr, args, rt, di.cases, di.term)
						case dm: XMLMeansDefiniens => 								
						new MizFuncMeansDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.kind, d.nr, args, rt, dm.cases, dm.form)
						}
						art.addElem(f)
					}
					case None => println("Err in Controller -> buildDefinitions : Func with no return type")
					}

				}
				case "M"  => {
					val m = df match { 
					case di  : XMLIsDefiniens => 								
					new MizModeIsDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, d.retType, di.cases, di.term)
					case dm: XMLMeansDefiniens => 								
					new MizModeMeansDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, d.retType, dm.cases, dm.form)
					}
					art.addElem(m)
				}
				case "V"  => {
					d.retType match {
					case Some(rt) => {
						val f = df match { 
						case di  : XMLIsDefiniens => 								
						new MizAttrIsDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, rt, di.cases, di.term)
						case dm: XMLMeansDefiniens => 								
						new MizAttrMeansDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, rt, dm.cases, dm.form)
						}
						art.addElem(f)
					}
					case None => println("Err in Controller -> buildDefinitions : Attribute with no return type")
					}
				}
				case _  => {
					println("Err in Controller -> buildDefinitions : def kind not supported yet : " + d.kind)
				}

				}
			}
			case None => println("Err in Controller -> buildDefinitions : unable to parse arg names")
			}

	}
	
	/*
	def addIsDefiniens(d : XMLIsDefiniens) : Unit = {
		currentDefBlock match {
			case Some(db) => {
				db.defs.find(x => (x.nr == d.absconstrnr && x.kind == d.constrkind && x.aid == d.constraid)) match {
					case Some(df) => {
						currentArticle match {
							case Some(art) => {
								df.kind match {
									case "M" =>
										val opArgs = df.resolveArgs()
										opArgs match {
											case Some(args) =>
												val m = new MizModeIsDef(resolveDef(df.aid, df.kind, df.nr), df.aid, df.nr, args, df.retType, d.cases, d.term)
												art.addElem(m)
											case None => println("Err in Controller -> addIsDefiniens : unable to parse Mode arg names")
										}
									case "K" | "O" =>
										df.retType match {
											case Some(rt) => 
												val opArgs = df.resolveArgs()
												opArgs match {
												case Some(args) =>
													val f = new MizFuncIsDef(resolveDef(df.aid, df.kind, df.nr), df.aid, df.nr, args, rt, d.cases, d.term)
													art.addElem(f)
												case None => println("Err in Controller -> addIsDefiniens : unable to parse Func arg names")
											}												
											case None => println("err in Controller -> addIsDefiniens : Func with no return type")												
										}
									case "R" => 
										val opArgs = df.resolveArgs()
										opArgs match {
											case Some(args) =>
												val p = new MizPredIsDef(resolveDef(df.aid, df.kind, df.nr), df.aid, df.nr, args, d.cases, d.term)
												art.addElem(p)
											case None => println("Err in Controller -> addIsDefiniens : unable to parse Mode arg names")
										}
									case "V" => 
										df.retType match {
											case Some(rt) => 
												val opArgs = df.resolveArgs()
												opArgs match {
												case Some(args) => 
													val p = new MizAttrIsDef(resolveDef(df.aid, df.kind, df.nr), df.aid, df.nr, args, rt, d.cases, d.term)
													art.addElem(p)
												case None => println("Err in Controller -> addMeansDefiniens : unable to parse Attribute arg names")
											}
											case None => println("err in Controller -> addIsDefiniens : Attr with no return type")												
										}
								}
							}
							case None => println("err -> article not init for MizIsDefiniens: " + d.aid + d.constrkind + d.absconstrnr)
						}
					}
					case None => println("err -> no constructor found for: " + d.aid + d.constrkind + d.absconstrnr)
				}
			}
			case None => println("err -> no currentDefBlock for -> " + d.aid + d.constrkind + d.absconstrnr)
		}
	}
	
	def addMeansDefiniens(d : XMLMeansDefiniens) : Unit = {
		currentDefBlock match {
			case Some(db) => {
				db.defs.find(x => (x.nr == d.absconstrnr && x.kind == d.constrkind && x.aid == d.constraid)) match {
					case Some(df) => {
						currentArticle match {
							case Some(art) => {
								df.kind match {
									case "M" =>
										val opArgs = df.resolveArgs()
										opArgs match {
											case Some(args) =>
												val m = new MizModeMeansDef(resolveDef(df.aid, df.kind, df.nr), df.aid, df.nr, args, df.retType, d.cases, d.form)
												art.addElem(m)
											case None => println("Err in Controller -> addMeansDefiniens : unable to parse Mode arg names")
										}
									case "K" | "O" =>
										df.retType match {
											case Some(rt) => 
												val opArgs = df.resolveArgs()
												opArgs match {
												case Some(args) =>
													val f = new MizFuncMeansDef(resolveDef(df.aid, df.kind, df.nr), df.aid, df.nr, args, rt, d.cases, d.form)
													art.addElem(f)
												case None => println("Err in Controller -> addMeansDefiniens : unable to parse Func arg names")
											}												
											case None => println("err in Controller -> addMeansDefiniens : Func with no return type")												
										}
									case "R" => 									
										val opArgs = df.resolveArgs()
										opArgs match {
											case Some(args) =>
												val p = new MizPredMeansDef(resolveDef(df.aid, df.kind, df.nr), df.aid, df.nr, args, d.cases, d.form)
												art.addElem(p)
											case None => println("Err in Controller -> addMeansDefiniens : unable to parse Predicate arg names")
										}
									case "V" => 
										df.retType match {
											case Some(rt) => 
												val opArgs = df.resolveArgs()
												opArgs match {
												case Some(args) => 
													val p = new MizAttrMeansDef(resolveDef(df.aid, df.kind, df.nr), df.aid, df.nr, args, rt, d.cases, d.form)
													art.addElem(p)
												case None => println("Err in Controller -> addMeansDefiniens : unable to parse Attribute arg names")
											}
											case None => println("err in Controller -> addIsDefiniens : Attr with no return type")												
										}
										
								}
							}
							case None => println("err -> article not init for MizMeanssDefiniens: " + d.aid + d.constrkind + d.absconstrnr)
						}
					}
					case None => println("err -> no constructor found for: " + d.aid + d.constrkind + d.absconstrnr)
				}
			}
			case None => println("err -> no currentDefBlock for -> " + d.aid + d.constrkind + d.absconstrnr)
		}
	}
	*/

	def resolveDef(aid : String, kind : String, absnr : Int) : Option[String] = {
		dictionary.symbols.find(x => ((x.aid == Some(aid)) && (x.kind == kind) && (x.absnr == absnr))) match {
		  case Some(x) => 
			Some(x.name)
			None //TODO remove for notations		  
		  case None => 
		  //println(aid + kind + absnr)
		  //dictionary.symbols.filter(x => x.aid != None).map(s => (println(s.name + " = " + s.aid + " " + s.kind + " " + s.absnr + " " + s.formatnr + " " + s.symbolnr)))
		  None
		}
	}

	def resolveVar(Svid : String) : Option[String] = {
		try {
			val vid = Svid.toInt
			dictionary.symbols.find(x => x.symbolnr == vid && (x.kind == "I")) match {
				case Some(x) => Some(x.name)
				None //TODO remove for notations
				case None => 
				None
			}
		} catch {
			case e : Exception => 
			None
		}
	}
	
	def resolveSelector(aid : String, absnr : Int) : String = {
	  "a"
	}
	
}
