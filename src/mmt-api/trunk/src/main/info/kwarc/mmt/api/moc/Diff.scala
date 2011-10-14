package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.modules._
import scala.collection.mutable._
import scala.xml.Node


object Differ {
  
	def diff(cold : Controller, cnew : Controller, pold : DPath, pnew : DPath) : Diff = {
	  val old = cold.getDocument(pold)	
	  val nw = cnew.getDocument(pnew)
	  val change = compareDocuments(old, nw, cold, cnew)
	  var diff = new Diff(cold, cnew, pold, pnew)
	  diff.changes = List(change)
	  diff
	}
  
	def applyDiff(diff : Diff, controller : Controller) : Document = {
	  val baseDoc = controller.getDocument(diff.old)
	  diff.changes.map(x => x match {
	    case c : ChangeDocument => applyDocChange(c, diff.cold, controller)
	    case c : ChangeModule => applyModuleChange(c, diff.cold, controller)
	    case c : ChangeDeclaration => applyDeclarationChange(c, diff.cold, controller)
	  })
	  
	  controller.getDocument(diff.nw)
	}
	
	
	def genCSS(c : ChangeModule, fname : String) = {
	  val css = new java.io.FileWriter("/home/mihnea/public_html/" + fname + ".css")
	  val js = new java.io.FileWriter("/home/mihnea/public_html/" + fname + ".js")
	  js.write("window.onload = function() {\n")
	  mod2CSS(c ,css, js)
	  js.write("\n}")
	  css.close()
	  js.close()
	  
	}
	
	def comp2CSS(path: Path, ch : ChangeComponent, nr : Int, css : java.io.FileWriter, js : java.io.FileWriter) = {
	  val trClass = nr match {
	    case 0 => "omdoc-symbol"
	    case 1 => "omdoc-definitiens"
	  }
	  
	  ch match {
	  case c : AddComponent => 	   	
	    css.write("tr." + trClass + "[id=\"" + path.toPath + "\"] td:nth-child(" + 3 + ")\n{\ncolor:blue;\n}\n")
	  case c : UpdateComponent =>	   	
	    css.write("tr." + trClass + "[id=\"" + path.toPath + "\"] td:nth-child(" + 3 + ")\n{\ncolor:#00DDDD;\n}\n")
	  case c : DeleteComponent =>	   	
	    css.write("tr." + trClass + "[id=\"" + path.toPath + "\"] td:nth-child(" + 3 + ")\n{\ncolor:red;\n}\n")
	  case c : IdenticalComponent => None
	  }
	}
	def decl2CSS(ch : ChangeDeclaration, css : java.io.FileWriter, js : java.io.FileWriter) = ch match {
	  case c : AddDeclaration => 
	    val el = "tr.omdoc-symbol[id=\\\"" + c.d.path.toPath + "\\\"]"
	    css.write("tr.omdoc-symbol[id=\"" + c.d.path.toPath + "\"]\n{\ncolor:blue;\n}\n")
	    c.tp match {
	      case "Constant" => js.write("document.querySelectorAll(\"" + el + "\")[0].setAttribute(\"title\", \"Added Constant\")\n")
	      case _ => 
	    }
	  case c : UpdateDeclaration => 
	    val el = "tr.omdoc-symbol[id=\\\"" + c.path.toPath + "\\\"] td:nth-child(1)"
	   	css.write("tr.omdoc-symbol[id=\"" + c.path.toPath + "\"] td:nth-child(1)\n{\ncolor:#00DDDD;\n}\n")
	   	c.tp match {
	      case "Constant" => js.write("document.querySelectorAll(\"" + el + "\")[0].setAttribute(\"title\", \"Updated Constant\")\n")
	      case _ => 
	    }
	    c.changes.zipWithIndex.map(p => comp2CSS(c.path, p._1, p._2, css, js))
	  case c : RenameDeclaration =>
	    val el = "tr.omdoc-symbol[id=\\\"" + (c.path.parent % c.name)  + "\\\"] td:nth-child(1)"
	    css.write("tr.omdoc-symbol[id=\"" + (c.path.parent % c.name)  + "\"] td:nth-child(1)\n{\ncolor:#EEBB00;\n} \n")
	    c.tp match {
	      case "Constant" => js.write("document.querySelectorAll(\"" + el + "\")[0].setAttribute(\"title\", \"Renamed Constant (from " + c.path.name + ")\")\n")
	      case _ =>
	    }
	    case c : DeleteDeclaration =>
	    js.write("var result = document.querySelectorAll(\"div > table\")\n" + 
                 "var res = result[0];\n" +
                 "var tr = document.createElement(\"tr\")\n" +
                 "var td = document.createElement(\"td\")\n" +
                 "td.innerHTML = \"" + c.path.name + "\"\n" +
                 "tr.setAttribute(\"id\",\"" + c.path.toPath +"\")\n" +
                 "tr.setAttribute(\"class\",\"omdoc-symbol\")\n" +
                 "tr.appendChild(td)\n" +
                 "res.appendChild(tr)\n") 
        val el =  "tr.omdoc-symbol[id=\\\"" + c.path.toPath + "\\\"]"        
	  	css.write("tr.omdoc-symbol[id=\"" + c.path.toPath + "\"]\n{\ncolor:red;\nbackground:lightgray;\n}\n")	   
	  	c.tp match {
	      case "Constant" => js.write("document.querySelectorAll(\"" + el + "\")[0].setAttribute(\"title\", \"Deleted Constant (retrieved from old version)\")\n")
	      case _ => 
	    }
	  case c : IdenticalDeclaration => 
	    val el =  "tr.omdoc-symbol[id=\\\"" + c.path.toPath + "\\\"] td:nth-child(1)"        
	  	c.tp match {
	      case "Constant" => js.write("document.querySelectorAll(\"" + el + "\")[0].setAttribute(\"title\", \"Identical Constant (no change)\");\n")
	      case _ =>
	    }
	}
	
	def mod2CSS(ch : ChangeModule, css : java.io.FileWriter, js : java.io.FileWriter) = ch match {
	  case c : AddModule => 
	  	css.write("div[id=\"" + c.m.path.toPath + "\"]\n{\ncolor:blue;\n}\n")
	  	js.write("document.querySelectorAll(\"div[id=\\\"" + c.m.path.toPath + "\\\"] span:first-of-type\")[0].setAttribute(\"title\", \"Added Module \");\n")
	  case c : UpdateModule => 
	    css.write("div[id=\"" + c.path.toPath + "\"]\n{\ncolor:#00DDDD;\n}\n")
	    js.write("document.querySelectorAll(\"div[id=\\\"" + c.path.toPath + "\\\"] span:first-of-type\")[0].setAttribute(\"title\", \"Updated Module \");\n")
	    c.componentChanges.map(decl2CSS(_, css, js))
	  case c : RenameModule =>
	  	css.write("div[id=\"" + (c.path.parent ? c.name).toPath + "\"]\n{\ncolor:#EEBB00;\n}\n")
	  	js.write("document.querySelectorAll(\"div[id=\\\"" + (c.path.parent ? c.name).toPath + "\\\"] span:first-of-type\")[0].setAttribute(\"title\", \"Renamed Module (from " + c.path.name + ")\");\n")
	  case c : DeleteModule =>
	  	css.write("div[id=\"" + c.path.toPath + "\"]\n{\ncolor:red;\n}\n")
	  case c : IdenticalModule => None
	}
	
	
	def  applyDocChange(cd : ChangeDocument, cold : Controller, controller : Controller) : Unit = {
	  cd match {
	    case AddDocument(d) => controller.add(d)
	    case UpdateDocument(path, changes) => 
	     val doc = new Document(path)
	     
	     controller.add(doc)
	     changes.map( _ match { 
	      case mc : ChangeModule => applyModuleChange(mc, cold, controller) 
	      case dc : ChangeDocument => applyDocChange(dc, cold, controller)
	    })
	    case DeleteDocument(path) => None
	    case RenameDocument(path, name) => None
	    case IdenticalDocument(path) => controller.add(cold.getDocument(path))
	  }
	}
	
	
	
	def applyModuleChange(cm : ChangeModule, cold : Controller, controller : Controller) = {
	  cm match {
	    case AddModule(m,tp) => (tp,m) match {
	        case ("DeclaredTheory",d : DeclaredTheory) => 
	        	val nd = new DeclaredTheory(d.parent, d.name, d.meta)
	        	controller.add(nd)
	        	controller.add(MRef(m.path.parent ,m.path, true))
	        	_declarations(d).map(x => controller.add(x))
	      }
	      
	    case UpdateModule(path,tp, bodyChanges, componentChanges) => 
	      val m = applyComponentChanges(cold.get(path), tp, componentChanges)
	      controller.add(m)
	      m.path match {
	        case pth : MPath => controller.add(MRef(m.path.doc ,pth, true))
	        case _ => 
	      }
	      bodyChanges.map(applyDeclarationChange(_, cold, controller))
	    case DeleteModule(path, tp) => None
	    case RenameModule(path, tp, name) =>  
	      (tp,cold.get(path)) match {
	        case ("DeclaredTheory",d : DeclaredTheory) => 
	        	val nd = new DeclaredTheory(d.parent, name, d.meta)
	        	controller.add(nd)
	        	controller.add(MRef(nd.path.parent ,nd.path, true))
	        	_declarations(d).map(x => x match {
	        	  case c : Constant => 
	        	    val nc = new Constant(OMMOD(c.home.toMPath.doc ? name), c.name, c.tp, c.df, c.rl)
	        	    controller.add(nc)
	        	  case i : Include => 
	        	    val in = new Include(OMMOD(i.home.toMPath.doc ? name), i.from)
	        	    controller.add(in)
	        	})
	      }
	      
	    case IdenticalModule(path, tp) => 
	      controller.add(cold.get(path))
	  }
	}
	
	def applyDeclarationChange(cd : ChangeDeclaration, cold : Controller, controller : Controller) = {
	  cd match {
	    case AddDeclaration(d, tp) => controller.add(d) 
	    case UpdateDeclaration(path, tp, changes) => {
	      val dcl = applyComponentChanges(cold.get(path),tp, changes)
	      controller.add(dcl)
	    }
	    case DeleteDeclaration(path, tp) => None
	    case RenameDeclaration(path, tp, name) => (tp, cold.get(path)) match {
	      case ("Constant", c : Constant) => 
	        val nd = new Constant(c.home, name, c.tp, c.df, c.rl)
	        controller.add(nd)
	    }
	    case IdenticalDeclaration(path, tp) => controller.add(cold.get(path))
	  }
	}
	
	def applyComponentChanges(s : StructuralElement, tp : String, componentChanges : List[ChangeComponent]) : StructuralElement = {
	  (tp,s) match {
	    case ("Constant", c : Constant) => 
	      val home = c.home
	      val name = c.name
	      var tp = c.tp
	      var df = c.df
	      var rl = c.rl
	      componentChanges.map(x => x match {
	        case AddComponent(name, comp) => (name, comp) match {
	          case ("type", Obj2Component(t : Term)) => tp = Some(t)
	          case ("definition", Obj2Component(t : Term)) => df = Some(t)
	          case _ => throw ImplementationError("match error in Constant components")
	        }   
	        case DeleteComponent(ctp,name) => (name) match {
	          case "type" => tp = None
	          case "definition" => df = None
	          case _ => throw ImplementationError("match error in Constant components")
	        }
	        case UpdateComponent(name,comp) => (name,comp) match {
	          case ("type", Obj2Component(t : Term)) => tp = Some(t)
	          case ("definition", Obj2Component(t : Term)) => df = Some(t)
	          case _ => throw ImplementationError("match error in Constant components")

	        }
	        case IdenticalComponent(ctp,name) => None //already set above
	      })
	      val cn = new Constant(home, name, tp, df, rl)
	      cn
	    case ("DeclaredTheory", d : DeclaredTheory) => 
	      val doc = d.parent
	      val name = d.name
	      var meta = d.meta
	      componentChanges.map(x => x match {
	        case AddComponent(name, comp) => (name, comp) match {
	          case ("meta", Path2Component(p : MPath)) => meta = Some(p)
	          case _ => throw ImplementationError("match error in Declared Theory components")

	        }   
	        case DeleteComponent(ctp,name) => (name) match {
	          case "meta" => meta = None
	          case _ => throw ImplementationError("match error in Declared Theory components")
	        }
	        case UpdateComponent(name,comp) => (name,comp) match {
	          case ("meta", Path2Component(p : MPath)) => meta = Some(p)
	          case _ => throw ImplementationError("match error in Declared Theory components")

	        }
	        case IdenticalComponent(ctp,name) => None //already set above
	      })
	      
	      val dn = new DeclaredTheory(doc, name, meta)
	      dn
	  }
	}
	
	
	
	
	
	def _getType(s : StructuralElement) : String = s match {
	  case t : DeclaredTheory => "DeclaredTheory"
	  case t : DefinedTheory => "DefinedTheory"
	  case c : Constant => "Constant"
	  case a : Assignment => "Assignment"
	  case i : Include => "Include"
	}
	
	def compareDeclarations(old : Declaration, nw : Declaration) : ChangeDeclaration = {
	  (old,nw) match {
	    case (o : Constant, n : Constant) => 
	      var changed = false
	      val tp = (o.tp,n.tp) match {
	        case (None,None) => IdenticalComponent("Term","type")
	        case (Some(s),None) => 
	          changed = true
	          DeleteComponent("Term","type")
	        case (None,Some(s)) => 
	          changed = true
	          AddComponent("type", Obj2Component(s))
	        case (Some(oc),Some(nc)) => 
	          if (oc == nc)
	            IdenticalComponent("Term","type")
	          else {
	            changed = true
	            UpdateComponent("type", Obj2Component(nc))
	          }
	      }
	      
	      val df = (o.df,n.df) match {
	        case (None,None) => IdenticalComponent("Term", "definition")
	        case (Some(s),None) => 
	          changed = true
	          DeleteComponent("Term", "definition")
	        case (None,Some(s)) =>
	          changed = true
	          AddComponent("definition", Obj2Component(s))
	        case (Some(oc),Some(nc)) => 
	          if (oc == nc)
	            IdenticalComponent("Term", "definition")
	          else {
	            changed = true
	            UpdateComponent("definition", Obj2Component(nc))
	          }
	      }
	      changed match {
	        case true => UpdateDeclaration(n.path, "Constant", List(tp,df))
	        case false => IdenticalDeclaration(n.path, "Constant")
	      }
	    case (o : Include, n : Include) =>
	      var changed = false
	      val from = if (o.from == n.from)
	            IdenticalComponent("TheoryObj", "from")
	          else {
	            changed = true
	            UpdateComponent("from", TheoryObj2Component(n.from))
	          }
	      changed  match {
	        case true => UpdateDeclaration(n.path, "Include", List(from))	      
	        case false => IdenticalDeclaration(n.path, "Include")
	      }
	  }
	}
	
	
	def _getDeclFields(d : Declaration, fields : List[String]) : List[Node] = {
		d match {
		  case c : Constant => fields.map(s => s match {
		    case "type" => 
		      <constant-tp-update>
		    	{c.tp match {case Some(t) => t.toNode case None => }}
		      </constant-tp-update>
		    case "definition" => 
		       <constant-df-update>
		    	{c.df match {case Some(t) => t.toNode case None => }}
		      </constant-df-update>
		  })
		  case i : Include => Nil
		}
	  
	}
	
	
	def _declarations(m : Module) : List[Declaration] = {
	  m.components.flatMap(x => x match {
	    case d : Declaration => List(d)
	    case _ => Nil
	  })
	}
	
	def _checkDocumentRenames(nw : Document, options : List[(XRef,Document)]) : (Document, Option[(XRef,Document)]) = {
	   val matches = options.flatMap(old => {
	     (old._2.getItems == nw.getItems)  match {
	          case true => List(old)
	          case false => Nil
	        }
	    })
	  
	  matches.length match {
	    case 1 => (nw,Some(matches(0)))
	    case _ => (nw,None)
	  }
	}
	
	
	def _checkModuleRenames(nw : Module, options : List[(XRef,Module)]) : (Module, Option[(XRef,Module)]) = {
	   val matches = options.flatMap(old => {
	    (old._2,nw) match {
	      case (o : DeclaredTheory, n : DeclaredTheory) =>
	        val od = _declarations(o)
	        val nd = _declarations(n)
	        val decl = od.zip(nd).map(p => compareDeclarations(p._1, p._2)).foldRight(true)((x,r) => x match {
	          case d : IdenticalDeclaration => r && true
	          case _ => false
	        })
	        (o.meta == n.meta) && (decl) match {
	          case true => List((old._1,o))
	          case false => {
	            Nil
	          }
	        }
	      case _ => Nil
	    }
	  })

	  matches.length match {
	    case 1 => (nw,Some(matches(0)))
	    case _ => (nw,None)
	  }
	}
	
	
	def _checkDeclarationRenames(nw : Declaration, options : List[Declaration]) : (Declaration,Option[Declaration]) = {
	  val matches = options.flatMap(old => {
	    (old,nw) match {
	      case (o : Constant, n : Constant) => 
	        (o.df == n.df) && (o.tp == n.tp) match {
	          case true => List(o)
	          case false => Nil
	        }
	      case _ => Nil
	    }
	  })
	  
	  matches.length match {
	    case 1 => (nw,Some(matches(0)))
	    case _ => (nw,None)
	  }
	  
	}
	
	def _max(a : Int, b : Int) : Int = {
	  if (a > b){
	    a
	  }
	  else { 	 
		b
	  }
	}
	
	def _rank(changes : List[Change]) : Int = {
		changes.foldRight(0)((c,r) => c match {
		  case x : Add => _max(r,1)
		  case x : Update => _max(r,1)
		  case x : Rename => _max(r,2)
		  case x : Delete => _max(r,2)
		  case x : Identical => _max(r,0)
		})
	}
	
	
	def compareModules(old : Module, nw : Module) : ChangeModule = {
	  (old,nw) match {
	    case (o : DeclaredTheory, n : DeclaredTheory) =>
	    	val od = _declarations(o)
	    	val nd = _declarations(n)
	    	val r = nd.flatMap(x => od.filter(y => x.name == y.name))
	    
	    	var pairs = new scala.collection.mutable.ListMap[Declaration,Int]
	    	var vals = new scala.collection.mutable.LinkedList[Option[ChangeDeclaration]]
	    	nd.map(x => {
	         pairs(x) = vals.length
	    	 vals = vals :+ None

	    	})
	    	
	    	var used = new scala.collection.mutable.HashMap[Declaration,Boolean]
	    	od.map(x => used(x) = false)
	    	
	    	//checking for old-new pairs by name
	    	pairs.map(p => od.find(y => p._1.name == y.name) match {
	    	  case Some(r) =>
	    	    used(r) = true
	    	    vals(p._2) = Some(compareDeclarations(r,p._1))
	    	  case None => None    	  
	    	})
	    
	    	//checking for renames by searching through adds and deletes for matches
	    	val tmpadds = pairs.filter(p => vals(p._2) match {
	    		case None => true
	    		case Some(d) => false
	    	})
	      
	    	val tmpdels = used.filterNot(_._2).map(_._1).toList 
	    	tmpadds.map(x => _checkDeclarationRenames(x._1,tmpdels)).map(p => {
	    		p._2 match {
	    		case Some(x) =>
	    		  	used(x) = true
	    			vals(pairs(p._1)) = Some(RenameDeclaration(x.path, _getType(p._1), p._1.name))
	    		case None => None
	    		}
	    	})
	      
	       

	      val deletes = used.filterNot(_._2).map(_._1).map(d => DeleteDeclaration(d.path, _getType(d)))
	      pairs.map(p => vals(p._2) match {case Some(s) => None case None => vals(p._2) = Some(AddDeclaration(p._1, _getType(p._1)))})
	      
	      val changes = (vals.flatMap(v => v match {case Some(s) => List(s) case None => Nil}) ++ deletes).toList
	     
	      
	      val meta = (o.meta,n.meta) match {
	    	  case (None,None) => IdenticalComponent("Path","meta")
	    	  case (None,Some(p)) => AddComponent("meta",Path2Component(p))
	    	  case (Some(p),None) => DeleteComponent("Path","meta")
	    	  case (Some(op),Some(np)) => 
	    	    if (op == np) 
	    	      IdenticalComponent("Path", "meta") 
	    	    else 
	    	      UpdateComponent("meta", Path2Component(np))
	      }
	    
	      _rank(meta :: changes) match {
	        case 0 => IdenticalModule(n.path, _getType(n))
	        case _ => UpdateModule(n.path, _getType(n), changes, List(meta))
	      }
	  }
	}
	
	def compareDocuments(old : Document, nw : Document, cold : Controller, cnew : Controller) : ChangeDocument = {
	  val od = old.getItems
	  val nd = nw.getItems
	  
	  var pairs = new scala.collection.mutable.ListMap[XRef,Int]
	  var vals  = new scala.collection.mutable.LinkedList[Option[Change]] 
	  
	  nd.map(x => {
		  pairs(x) = vals.length
		  vals = vals :+ None
	  })
	  
	  var used = new scala.collection.mutable.HashMap[XRef,Boolean]
	  od.map(x => used(x) = false)
	  
	  //checking for old-new pairs by name
	  pairs.map(p => od.find(y => p._1.target == y.target) match {
	  	case Some(r) =>
	  		used(r) = true
	  		(cold.get(r.target),cnew.get(p._1.target)) match {
	  		  case (omd : Module, nmd : Module) => vals(pairs(p._1)) = Some(compareModules(omd,nmd))
	  		  case (odc : Document, ndc : Document)  => vals(pairs(p._1)) = Some(compareDocuments(odc,ndc, cold, cnew))
	  		}
	  	case None => None    	  
	  })
	   
	  
	  //checking for renames by searching through adds and deletes for matches
	  val tmpadds = pairs.filter(p => vals(p._2) match {
	  case None => true
	  case Some(d) => false
	  })

	  var tmpModDels = new scala.collection.mutable.LinkedList[(XRef,Module)]
	  var tmpDocDels = new scala.collection.mutable.LinkedList[(XRef,Document)]
	  
	  used.filterNot(_._2).map(x => cold.get(x._1.target) match {
	    case m : Module => tmpModDels = tmpModDels :+ (x._1,m)
	    case d : Document => tmpDocDels = tmpDocDels :+ (x._1,d)
	  })
	  
	  
	  
	  tmpadds.map(x => cnew.get(x._1.target) match {
	    case mdl : Module => _checkModuleRenames(mdl, tmpModDels.toList) match {
	      case (nwm,Some(odm)) => 
	        vals(pairs(x._1)) = Some(RenameModule(odm._2.path, _getType(odm._2), nwm.name))
	        used(odm._1) = true
	      case _ => None
	    }
	    
	    case dcm : Document => _checkDocumentRenames(dcm, tmpDocDels.toList) match {
	      case (nwd,Some(odd)) => 
	        vals(pairs(x._1)) = Some(RenameDocument(odd._2.path, nwd.path.toPath))
	        used(odd._1) = true
	      case _ => None
	    }
	  })
	      
	  val deletes =  used.filterNot(_._2).map(_._1).map(x => cold.get(x.target) match {
	    case mdl : Module => DeleteModule(mdl.path, _getType(mdl))
	    case doc : Document => DeleteDocument(doc.path)
	  })
	  
	  
	  pairs.map(p => vals(p._2) match {case Some(s) => None case None => vals(p._2) = cnew.get(p._1.target) match {
	    case dcm : Document => Some(AddDocument(dcm))
	    case mdl : Module => Some(AddModule(mdl, _getType(mdl)))
	    }})
	      
	  val changes = (vals.flatMap(v => v match {case Some(s) => List(s) case None => Nil}) ++ deletes).toList

	  	      
	  _rank(changes) match {
	  	case 0 => IdenticalDocument(nw.path)
	  	case _ => UpdateDocument(nw.path, changes)
	  }
	}
	

}











/*

class Differ {
  
  def applyChange(c : Change, controller : Controller)  = {
    c match {
      case Add(e) => controller.add(e)
      case Update(e) => {
        controller.get(e.parent) match {
          case t : DeclaredTheory => 
            (controller.get(e.path),e) match {
              case (s : Symbol, nw : Symbol) => t.replace(s,nw)
              case _ => //this should not happen since parent(e) is a declared theory
            }
          case _ => 
        }
      }
      
      case Delete(p) => {
        controller.library.delete(p)
      }
      case Rename(p, nw_name) => {
        val s = controller.get(p)
        controller.library.delete(p)
        s match {
          case c : Constant => 
            val nw_c = new Constant(c.home, nw_name, c.tp, c.df, c.uv)
            controller.add(nw_c)
          case _ => println("Warning in Diff-> applyChange -> Rename : Symbol type not supported")
        }
      }
    }
  }
 
  
  
  def applyDiff(diff : Diff, controller : Controller) : Document = {
	val doc = diff.old.getDocument(diff.path)
	
	controller.add(doc)
	doc.getModulesResolved(diff.old.library).map(x => { 
	  x match {
	    case t : DeclaredTheory =>
	      val nw_t = 
	        new DeclaredTheory(t.parent, t.name, t.meta)
	      	controller.add(nw_t)
	      	t.innerComponents.map(e => controller.add(e))
	    case _ =>  None
	  }
	}
	  )
	
	diff.changes.map(c => applyChange(c : Change, controller : Controller))
	doc
  }
  
  
  def components(s : StructuralElement) : List[StructuralElement] = {
    val l = s.components.flatMap(x => x match {
      case se : StructuralElement => List(se)
      case _ => Nil
    })
    l
  }
  
   def diff(cold : Controller, cnew : Controller, p : DPath) : Diff = {
  
     val dold = cold.getDocument(p)
     val dnew = cnew.getDocument(p)
     
     val status = dnew.getModulesResolved(cnew.library).flatMap(x => dold.getModulesResolved(cold.library).map(y => compare(y, x)))
     
     Diff(cold, cnew,p, status.flatMap(x => x.toChange).toSeq : _*)
   }
      
   def compare(old : StructuralElement, nw : StructuralElement) : Status = {
     //println(old.toString)
     //println(nw.toString)
     
     (old,nw) match {
       case (so : Symbol,sn : Symbol) =>
         compareSymbols(so, sn)
       case (mo : Module, mn : Module) => 
         compareModules(mo,mn)
       case _ => new Distinct(nw)
     }
     
   }
   	
   def compareModules(old : Module, nw : Module) : Status = {
     //println("old")
     //components(old).map(println)
     //println("new")
     //components(nw).map(println)
     val r = components(nw).map(x => components(old).foldLeft[Status](new Distinct(nw))((res,y) => 
       { res match {
         case Distinct(_) => compare(y,x)
         case _ => res
       }
       }))
       
       val used = r.flatMap( x => x match {
         case Identical(p) => List(p)
         case Modified(_,p) => List(p)
         case Distinct(c) => Nil 
       })      
       
       val unused = components(old).filter(x => !used.contains(x.path))
       val deletes = unused.map(x => Delete(x.path))
       val changes : List[Change] = r.flatMap(x => x.toChange) ::: deletes
       Modified(changes, old.path)
   }
   
   def compareSymbols(old : Symbol, nw : Symbol) : Status = {
     (old, nw) match {
       case (oc : Constant, nc : Constant) => {
        compareConstants(oc, nc)
       }
       case (oc : Include, nc : Include) => {
         compareIncludes(oc,nc)
       }
       case _ => 
         //println("old: " + old.toString)
         //println("new: " + nw.toString)
         new Distinct(nw)
     }
   }
   
   
   
   def compareConstants(old : Constant, nw : Constant) : Status = {
      
    val name = (old.name == nw.name)
    val tp = (old.tp == nw.tp)
    val df = (old.df == nw.df)
    
    (name,tp,df) match {
      case (true,true,true) => new Identical(old.path)
      case (false,true,true) => new Modified(List( Rename(old.path, nw.name)),old.path)
      case (true,_,_) => new Modified( List( Update(nw)),old.path)
      case _ => new Distinct(nw)
    }
   
   }
   
   def compareIncludes(old : Include, nw : Include) : Status = {
     val from  = old.from == nw.from
     val to = old.from == nw.from
     (from,to) match {
       case (true, _) => new Identical(old.path)
       case (false,_) => new Distinct(nw)
     }
   }
   
   /*
   def compareTerm(old : Term, nw : Term) : Status = {
     (old -> nw) match {
       case (OMA(of,ol), OMA(nf,nl)) => 
         if (of == nf && ol == nl) {
        	 new Identical(old) //TODO
         } else {
           new Distinct() //TODO
         }
       case (OMID(ogn), OMID(ngn)) => {
         if (ogn == ngn) {
           new Identical(ogn)
         } else {
           new Modified(Diff(new Update(nw)), ogn)
         }
       }
     }
     new Distinct()
   }
   */
   
}
*/