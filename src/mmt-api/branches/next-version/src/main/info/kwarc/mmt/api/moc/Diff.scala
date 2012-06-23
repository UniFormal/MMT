package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.libraries._

/*
import org.tmatesoft.svn.core._
import org.tmatesoft.svn.core.io._
import org.tmatesoft.svn.core.auth._
import org.tmatesoft.svn.core.wc._
*/

object Differ {
  
    def diff(c : Controller, p : MPath, rev : Int) : StrictDiff = {
      
      //println(p)
      val c2 = new Controller
      c2.backend.addStore(c.backend.copyStorages(rev) :_ *)
      
      val mold = c.get(p) match {
        case m : Module => m
        case _ => throw NotFound(p)
      }
      
      val mnew = c2.get(p) match {
        case m : Module => m
        case _ => throw NotFound(p)
      }
      compareModules(mold,mnew)
    }
    
	def diff(cold : Controller, cnew : Controller, pold : DPath, pnew : DPath) : StrictDiff = {

	  val old = cold.getDocument(pold)

	  val nw = cnew.getDocument(pnew)

	  //TODO
	  val mold = old.getModulesResolved(cold.library)(0)
	  val mnw = nw.getModulesResolved(cnew.library)(0)
	  compareModules(mold, mnw)
	}
	
	/*
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
	    c.childChanges.map(decl2CSS(_, css, js))
	  case c : RenameModule =>
	  	css.write("div[id=\"" + (c.path.parent ? c.name).toPath + "\"]\n{\ncolor:#EEBB00;\n}\n")
	  	js.write("document.querySelectorAll(\"div[id=\\\"" + (c.path.parent ? c.name).toPath + "\\\"] span:first-of-type\")[0].setAttribute(\"title\", \"Renamed Module (from " + c.path.name + ")\");\n")
	  case c : DeleteModule =>
	  	css.write("div[id=\"" + c.path.toPath + "\"]\n{\ncolor:red;\n}\n")
	  case c : IdenticalModule => None
	}	
	*/
		
	def _fullType(o : Obj) : List[String] = o.toNode.label :: o.toNode.attributes.toString :: Nil
	def _getType(s : StructuralElement) : String = s.toNode.label	
	
	
	def areEqual(old : Option[Obj], nw : Option[Obj]) : Boolean = (old,nw) match {
	  case (None,None) => true
	  case (Some(o), None) => false
	  case (None, Some(n)) => false
	  case (Some(o), Some(n)) => o == n
	}
	
	
	def compareObjects(o : Obj, n : Obj, pos : Position = Position(Nil)) : List[Position] = {
	  if (o == n) Nil
	  else if (_fullType(o) == _fullType(n) && o.components.length == n.components.length) {
	    o.components.zip(n.components).zipWithIndex.flatMap(p => p._1 match {
	      case (o1 : Obj, n1 : Obj) => compareObjects(o1, n1, pos + p._2)
	      case _ => if (p._1._1 == p._1._2) Nil else pos :: Nil
	    })
	  } else {
	    pos :: Nil
	  }
	}
	
	
	def compareConstants(o : Constant, n : Constant) : StrictDiff = {
    var changes : List[StrictChange] = Nil

    if(!areEqual(o.tp,o.df)) {
       changes = UpdateComponent(o.path, "type", o.tp, n.tp) :: changes
		}
		
		if (!areEqual(o.df, n.df))  {
		  changes = UpdateComponent(o.path, "def", o .df, n.df) :: changes
		}
		
		new StrictDiff(changes)
	}
	
  def compareStructures(o : Structure, n : Structure) : StrictDiff = {
	  var changes : List[StrictChange] = Nil

    if (o.from != n.from) {
	    changes = UpdateComponent(o.path, "from", Some(o.from), Some(n.from)) :: changes
	  }
    //TODO changes to body of DeclaredStructure, definiens of DefinedStructure
    new StrictDiff(changes)
	}
	
	def comparePatterns(old : Pattern, nw : Pattern) : StrictDiff = {
    var changes : List[StrictChange] = Nil

    if (old.params != nw.params){
      changes = UpdateComponent(old.path, "params", Some(old.params), Some(nw.params)) :: changes
    }

    if (old.body != nw.body) {
      changes = UpdateComponent(old.path, "body", Some(old.body), Some(nw.body)) :: changes
    }

	  new StrictDiff(changes)
	}
	
	def compareInstances(old : Instance, nw : Instance) : StrictDiff = {
    var changes : List[StrictChange] = Nil

	  if (old.pattern != nw.pattern) {
	    changes = UpdateComponent(old.path, "pattern", Some(OMID(old.pattern)), Some(OMID(nw.pattern))) :: changes
	  }

	  if (old.matches != nw.matches) {
      changes = UpdateComponent(old.path, "matches", Some(old.matches), Some(nw.matches)) :: changes
    }

    new StrictDiff(changes)
	}
	
	
	def compareConstantAssignments(old : ConstantAssignment, nw : ConstantAssignment) : StrictDiff = {
    var changes : List[StrictChange] = Nil

    if (old.target != nw.target) {
      changes = UpdateComponent(old.path, "target", Some(old.target), Some(nw.target)) :: changes
    }
	  
	  new StrictDiff(changes)
	}
	
	def compareDefLinkAssignments(old : DefLinkAssignment, nw : DefLinkAssignment) : StrictDiff = {
    var changes : List[StrictChange] = Nil

    if (old.target != nw.target) {
      changes = UpdateComponent(old.path, "target", Some(old.target), Some(nw.target)) :: changes
    }

	  new StrictDiff(changes)
	}
	
	def compareAliases(old : Alias, nw : Alias) : StrictDiff = {
    var changes : List[StrictChange] = Nil

    if (old.forpath != nw.forpath) {
      changes = UpdateComponent(old.path, "forpath", Some(OMID(old.forpath)), Some(OMID(nw.forpath))) :: changes
    }

	  new StrictDiff(changes)
	}

  def compareDeclarations(old : Declaration, nw : Declaration) : StrictDiff = {
    (old,nw) match {
      case (o : Constant, n : Constant) =>
        compareConstants(o,n)
      case (o : Structure, n : Structure) =>
        compareStructures(o,n)
      case (o : Pattern, n : Pattern) =>
        comparePatterns(o,n)
      case (o : Instance, n : Instance) =>
        compareInstances(o,n)
      case (o : ConstantAssignment, n : ConstantAssignment) =>
        compareConstantAssignments(o,n)
      case (o : DefLinkAssignment, n : DefLinkAssignment) =>
        compareDefLinkAssignments(o,n)
      case (o : Alias, n : Alias) =>
        compareAliases(o,n)
    }
  }

  def _declarations(m : Module) : List[Declaration] = {
	  m.components.flatMap(x => x match {
	    case d : Declaration => List(d)
	    case _ => Nil
	  })
	}
		
	
	def _max(a : Int, b : Int) : Int = {
	  if (a > b) a else b
	}
	
	def compareModules(old : Module, nw : Module) : StrictDiff = {
      //getting all declarations stored in each library

	  val od = _declarations(old)
	  val nd = _declarations(nw)
	  
	  // checking for declarations pairs having the same name aka same declaration with two versions
	  // due to name uniqueness max size of each filtered list is 0 or 1
	  // making the final result contain all declaration names that exist in both library versions
	  val matched = nd.flatMap(n => od.filter(o => n.name.toString == o.name.toString).map((_,n)))
	  
	  //filtering away matched paths
	  val unmatchedold = od.filterNot(x => matched.exists(y => x.name.toString == y._1.name.toString))
	  val unmatchednew = nd.filterNot(x => matched.exists(y => x.name.toString == y._1.name.toString))

    //generating adds & deletes
    val oldch : List[StrictChange] = unmatchedold.map(x => DeleteDeclaration(x))
    val newch : List[StrictChange] = unmatchednew.map(x => AddDeclaration(x))

	  //comparing declaration pairs to see how (if at all) they were updated over the two versions
	  val updates : List[StrictChange] = matched.flatMap(x => compareDeclarations(x._1,x._2).changes)
	  val innerChanges = new StrictDiff(updates ++ oldch ++ newch)
	  
	  (old,nw) match {
	    case (o : DeclaredTheory, n : DeclaredTheory) =>
        var changes : List[StrictChange] = Nil

        (o.meta, n.meta) match {
	    	  case (None,None) => None
	    	  case (None,Some(p)) => changes = UpdateComponent(o.path, "meta", None, Some(OMMOD(p))) :: changes
	    	  case (Some(p),None) => changes = UpdateComponent(o.path, "meta", Some(OMMOD(p)), None) :: changes
	    	  case (Some(op),Some(np)) =>
            if (op != np) {
	    	      changes = UpdateComponent(o.path, "meta", Some(OMMOD(op)), Some(OMMOD(np))) :: changes
            }
        }

	      new StrictDiff(changes) ++ innerChanges

	    case (o : DefinedTheory, n : DefinedTheory) =>
        var changes : List[StrictChange] = Nil

	      if (o.df != n.df) {
	        changes = UpdateComponent(o.path, "df", Some(o.df), Some(n.df)) :: changes
        }

	      new StrictDiff(changes) ++ innerChanges
	      
	    case (o : DeclaredView, n : DeclaredView) =>
        var changes : List[StrictChange] = Nil

        if (o.from != n.from) {
          changes = UpdateComponent(o.path, "from", Some(o.from), Some(n.from)) :: changes
        }

        if (o.to != n.to) {
          changes = UpdateComponent(o.path, "to", Some(o.to), Some(n.to)) :: changes
        }

        new StrictDiff(changes)  ++ innerChanges

	    case (o : DefinedView, n : DefinedView) =>
        var changes : List[StrictChange] = Nil

        if (o.from != n.from) {
          changes = UpdateComponent(o.path, "from", Some(o.from), Some(n.from)) :: changes
        }

        if (o.to != n.to) {
          changes = UpdateComponent(o.path, "to", Some(o.to), Some(n.to)) :: changes
        }

        if (o.df != n.df) {
          changes = UpdateComponent(o.path, "df", Some(o.df), Some(n.df)) :: changes
        }

	      new StrictDiff(changes)  ++ innerChanges
	  }
	}	
	
	
	def compareFlatLibraries(old : Library, nw : Library) : StrictDiff = {
	  
	  //getting all module URI's (paths) stored in each library
	  val ops = old.getAllPaths
	  val nps = nw.getAllPaths
	  
	  // checking for module pairs having the same URI aka same file with two versions
	  // due to path uniqueness max size of each filtered list is 0 or 1
	  // making the final result contain all paths that exist in both library versions
	  val matched = nps.flatMap(n => ops.filter(o => n.toPath == o.toPath))
	  
	  //filtering away matched paths
	  val unmatchedold = ops.filterNot(x => matched.exists(y => x.toPath == y.toPath))
	  val unmatchednew = nps.filterNot(x => matched.exists(y => x.toPath == y.toPath))
	  
	  //old module declarations become deletes
	  val oldch = unmatchedold.map(x => DeleteModule(old.getModule(x)))
	  
	  //new module declarations become adds
	  val newch = unmatchednew.map(y => AddModule(nw.getModule(y)))
	  
	  //comparing module pairs to see how (if at all) they were updated over the two versions
	  //since the match criterion was identical paths, we can reuse the path for both library versions
	  val updates = matched.map(x => compareModules(old.getModule(x), nw.getModule(x)))
	  updates.foldLeft(new StrictDiff(Nil))((r,x) => r ++ x) ++ new StrictDiff(oldch.toList) ++ new StrictDiff(newch.toList)
	}
	
}