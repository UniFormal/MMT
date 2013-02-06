package info.kwarc.mmt.mizar.mizar.objects

import scala.collection.mutable._
//import scala.collection.immutable._
/**
 * Trait for all Mizar classes
 */
trait MizAny {
  var sreg : Option[SourceRegion] = None
}

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
	var defBlockCounter = 0
	var definiens : HashMap[Int,LinkedList[XMLDefiniens]] = HashMap()
	var deftheorems : HashMap[Int,LinkedList[MizDefTheorem]] = HashMap()
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
	  val defBlockNr = defBlockCounter - 1 //they belong to the previous def block   
	  definiens(defBlockNr) :+= d 
	}

	def addDefTheorem(dt : MizDefTheorem) = {
	  val defBlockNr = defBlockCounter - 1 //they belong to the previous def block
	  deftheorems(defBlockNr) :+= dt 
	}
	
	def addDefinitionBlock(db : XMLDefinitionBlock) = {
	  deftheorems(defBlockCounter) = LinkedList() //deftheorems will be added as they follow the def block
	  definiens(defBlockCounter) = LinkedList() //definiens will be added as they follow the def block
	  defBlockCounter += 1	
	  elems = elems ++ db.defs 
	}
	
	def buildArticle() : MizArticle = {
	  val art = new MizArticle(currentAid, Nil)
	  elems.map({
	    case d : XMLDefinition =>
	      art.addElem(buildDefinition(d, art)) 
	    case x =>
	      art.addElem(x)
	  })
	  
	  elems = new LinkedList()
	  definiens = HashMap()
	  deftheorems = HashMap()
	  art
	}
	
	def buildDefinition(d : XMLDefinition, art : MizArticle) : MizDefinition = {
	  //getting matching definiens (0 or 1)
	  val (matchedDfns, unmatchedDfns) = definiens(d.defBlockNr).partition(x => (
	      x.constraid == d.defaid && 
	      x.constrkind == d.constrkind && 
	      x.absconstrnr == d.defnr))
	  
	  //assert(matchedDfns.length <= 1, "ParsingController.buildDefinition, found more than one " +
	  //		"(" + matchedDfns.length + ") matching definiens for " + d.defaid + d.constrkind + d.defnr)
	      
	  d.setDefiniens(matchedDfns.headOption)    
	  val leftOvers = if (matchedDfns.length > 1) matchedDfns.tail else Nil
	  
	  definiens(d.defBlockNr) = unmatchedDfns ++ leftOvers
	  //getting matching def theorems (0 to *)	  
	  val (matchedDts, unmatchedDts) = deftheorems(d.defBlockNr).partition(x => ( x.aid == d.aid &&
	      		                                       x.constrkind == d.constrkind &&
	      		                                       x.constrnr == d.constrnr))
	  deftheorems(d.defBlockNr) = unmatchedDts //removing used deftheorems
	  val dts = matchedDts.toList //matched def theorems will be added to the constructed definition
	  
	  //println(d.constraid + d.constrkind + d.absconstrnr + dts)
	  
	  val args = d.args.getOrElse(throw new Error("Controller.buildDefinition : no args found"))
	  val prefix = d.isRedef match {
	    case true => "Redef_" + d.kind + d.patternNr + "_of_" + d.defaid + "_"
	    case false => ""
	  }
	  
	  d.definiens match {
	    case Some(di : XMLIsDefiniens) => 
	      d.constrkind match {
	        case "R" => new MizPredIsDef(d.name, d.defaid, d.defnr, args, di.cases, di.term, dts, prefix)
	        case "K"|"O" => new MizFuncIsDef(d.name, d.defaid, d.constrkind, d.defnr, args, d.retType, di.cases, di.term, dts, prefix)
	        case "M" => new MizModeIsDef(d.name, d.defaid, d.defnr, args, d.retTypeO, di.cases, di.term, dts, prefix)
	        case "V" => new MizAttrIsDef(d.name, d.defaid, d.defnr, args, d.retType, di.cases, di.term, dts, prefix)
	      }
	    case Some(dm : XMLMeansDefiniens) =>
	      d.constrkind match {
	        case "R" => new MizPredMeansDef(d.name, d.defaid, d.defnr, args, dm.cases, dm.form, dts, prefix)
	        case "K"|"O" => new MizFuncMeansDef(d.name, d.defaid, d.constrkind, d.defnr, args, d.retType, dm.cases, dm.form, dts, prefix)
	        case "M" => new MizModeMeansDef(d.name, d.defaid, d.defnr, args, d.retTypeO, dm.cases, dm.form, dts, prefix)
	        case "V" => new MizAttrMeansDef(d.name, d.defaid, d.defnr, args, d.retType, dm.cases, dm.form, dts, prefix)
	      }
	    case None => //expandable mode  
	      if (d.isExp) {
	        new MizExpMode(d.name, d.aid, d.patternNr, args, d.getExpansion, dts)
	      } else if (d.isSuperfluous) {//superfluous redefinition
	        val args = d.args.getOrElse(throw new Error("Controller.buildDefinition for superfluous def and no args found")).map(_._2)
	        
	        //constructing definiens
	        val vars = for ((tp,i) <- args.zipWithIndex) yield new MizLocusVar(i + 1)
	        val term = new MizFunc(d.defaid, d.constrkind, d.defnr, vars)
	        val df = new XMLIsDefiniens(d.aid, 0, d.defaid, d.constrkind, d.defnr, Nil, Some(term), args)
	        definiens(d.defBlockNr) :+= df //adding reconstructed definiens
	        buildDefinition(d, art) //re-calling build	        
	      } else {
	        throw new Error("Controller.buildDefinition no definiens found, and not expandable mode" + d.defaid + d.constrkind + d.defnr)
	      }
	    case _ => throw new Error("Controller.buildDefinition: invalid definiens found " + d.toString)
	  }
	}
	
	
	/*
	def buildDefinition(d : XMLDefinition, art : MizArticle) :  Unit =  {
	  val df = definiens.find(x => (d.nr == x.absconstrnr && d.kind == x.constrkind && d.aid == x.constraid))  match {
	    case Some(dfn) => dfn
	    case None => d match {
	      case rd : XMLRedefinition => {
	        throw new Error("Err in Controller -> buildDefinitions : no definiens found for definition " + d.kind + d.nr)
	        new XMLIsDefiniens(d.aid, d.nr : Int, d.aid, d.kind, d.nr,Nil, 
	            Some(new MizFunc(rd.constraid, d.kind, rd.constrabsnr, 
	                rd.argTypes.zipWithIndex.map(x => new MizLocusVar(x._2 + 1)))), Nil)//TODO check
		  }
	      case _ => throw new Error("Err in Controller -> buildDefinitions : no definiens found for definition " + d.kind + d.nr)
	    }
	  }
	  
	  val dts = deftheorems.filter(x => (x.constrkind == d.kind && x.constrnr == d.relnr && x.constraid == d.aid)).toList
	  
	  val opArgs = d.resolveArgs()
	  opArgs match {
	    case Some(args) => {
	      d.kind match {
	        case "R"  => {
	          val p = df match {
	            case di  : XMLIsDefiniens =>
	              new MizPredIsDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, di.cases, di.term, dts)
	            case dm: XMLMeansDefiniens =>
	              new MizPredMeansDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, dm.cases, dm.form, dts)
	          }
	          p.sreg = d.sreg
	          art.addElem(p)
	        }
	        case "K" | "O" => 
	          val rt = d.retType.getOrElse(MizXML.set)
	              //throw new Error("Err in Controller -> buildDefinitions : Func with no return type " + d.kind + d.nr)
	          val f = df match {
	            case di  : XMLIsDefiniens =>
	              new MizFuncIsDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.kind, d.nr, args, rt, di.cases, di.term, dts)
	            case dm: XMLMeansDefiniens =>
	              new MizFuncMeansDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.kind, d.nr, args, rt, dm.cases, dm.form, dts)
	          }
	          f.sreg = d.sreg
	          art.addElem(f)
	        case "M"  => {
	          val m = df match {
	            case di  : XMLIsDefiniens =>
	              new MizModeIsDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, d.retType, di.cases, di.term, dts)
	            case dm: XMLMeansDefiniens =>
	              new MizModeMeansDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, d.retType, dm.cases, dm.form, dts)
	          }
	          m.sreg = d.sreg
	          art.addElem(m)
	        }
	        case "V"  => {
	          d.retType match {
	            case Some(rt) => {
	              if (args.last._2 != rt) {throw new Error("Ahaaaa")}
	            }
	            case None => //throw new Error("Err in Controller -> buildDefinitions : Attribute with no return type " + d.kind + d.nr)
	          }
	          val rt = args.last._2
	          val f = df match {
	            case di  : XMLIsDefiniens =>
	              new MizAttrIsDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, rt, di.cases, di.term, dts)
	            case dm: XMLMeansDefiniens =>
	              new MizAttrMeansDef(resolveDef(d.aid, d.kind, d.nr), d.aid, d.nr, args, rt, dm.cases, dm.form, dts)
	          }
	          f.sreg = d.sreg
	          art.addElem(f)
	        }
	        case _  => throw new Error("Err in Controller -> buildDefinitions : def kind not supported yet : " + d.kind)	
	      }
	    }
	    case None => throw new Error("Err in Controller -> buildDefinitions : unable to parse arg names" + d.kind + d.nr)
	  }
	}
	*/
	
	
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
