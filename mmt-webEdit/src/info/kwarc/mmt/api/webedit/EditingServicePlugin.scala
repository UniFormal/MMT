package info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.{Names, _}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web.ServerError


class EditingServicePlugin(val controller: Controller) {
  def getAutocompleteResponse(request: MMTAutoCompleteRequest): MMTAutoCompleteResponse = {
    val prefix = request.getPrefix
    val mpath = Path.parseM(request.getMPath, NamespaceMap(mmt.mmtbase))

    val myPaths = Names.resolve(OMMOD(mpath), Nil, prefix)(controller.globalLookup)
    val response = myPaths.map(_.path.toPath)
    new MMTAutoCompleteResponse(response)
  }

  def getResolveIncludesResponse(request: MMTResolveIncludesRequest) = {
    val symbol = request.getSymbol
    val mpath = Path.parseM(request.getMPath, NamespaceMap(mmt.mmtbase))

    val respMap = new collection.mutable.HashMap[String, Any]()
    Names.resolveIncludes(OMMOD(mpath), symbol)(controller.library) match {
      case None => respMap("found") = true
      case Some(myPaths) =>
        respMap("found") = false
        respMap("options") = myPaths.map(_.from.toPath)
    }
    new MMTResolveIncludesResponse(respMap.toList)
  }

  def minIncl(to: MPath): List[MPath] = {
    val thy = controller.get(to) match {
      case theor: DeclaredTheory => theor
      case _ => throw ServerError("No theory found")
    }
    val incl = thy.getIncludes
    //thy.components.length
    def remover(checked: List[MPath], rest: List[MPath]): List[MPath] = rest match {
      case Nil => checked
      case (hd :: tl) =>
        if (tl.exists(x => controller.globalLookup.visible(OMMOD(x)).contains(OMMOD(hd))) ||
          checked.exists(x => controller.globalLookup.visible(OMMOD(x)).contains(OMMOD(hd))))
          remover(checked, tl)
        else remover(hd :: checked, tl)
    }

    remover(Nil, incl)
  }


  def getMinIncludes(request: MMTMinIncludesRequest): MMTMinIncludesResponse = {
    val mpath = Path.parseM(request.getMPath, NamespaceMap(mmt.mmtbase))

    val newIncludes = minIncl(mpath)
    new MMTMinIncludesResponse(newIncludes.map(_.toString))
  }

  //  def getTermInference(request: MMTTermInferenceRequest) : MMTTermInferenceResponse = {
  //
  //      val mpath = Path.parseM(request.getMPath, mmt.mmtbase)
  //      val sref = new SourceRef(mpath.doc.uri, SourceRegion(SourcePosition(-1,0,0),SourcePosition(-1,0,0)))
  //
  //      val term = controller.textParser(ParsingUnit(sref, Context(), request.getTerm))(new ErrorLogger(controller.report))
  //
  //      def getHoles(term: Term , context : Context) : List[(Term,Context)] = {
  //        term match {
  //          case Hole(t) => (t,context)::Nil
  //          case OMBINDC(nterm,ncontext,nbodyList)=> (nterm::nbodyList).flatMap(getHoles(_,context++ncontext))
  //          case OMBIND(nterm,ncontext,nbody) => getHoles(nbody, context++ncontext)
  //          case OMA(f, args) => (f :: args).flatMap(getHoles(_, context))
  //          case _ => Nil
  //        }
  //      }
  //      //responses in cases
  //      val returnNoHoles = "Term Complete"
  //      val holeContextList =  getHoles(term,Context())
  //
  //      //response: Term Complete if there are no Holes, No Rules if no rule is applicable otherwise return the rules
  //      holeContextList match {
  //        case Nil => val returnNoHoles = "Term Complete" :: Nil
  //        			new MMTTermInferenceResponse(returnNoHoles)
  //        case _ =>
  //          val hole = holeContextList.head._1
  //          val context = holeContextList.head._2
  //          val prover = new Prover(controller)
  //          val stack = new Stack(context)
  //          val rules = prover.applicable(hole)(stack, null)
  //          rules match{
  //        			case Nil => val returnNoRules = "No Rules" :: Nil
  //        						new MMTTermInferenceResponse(returnNoRules)
  //        			case _ =>  new MMTTermInferenceResponse(rules.map(_.label.toString))
  //
  //          }
  //      }
  //  }

  def getSymbolCompletion(spathS: String): List[String] = {
    try {
      val spath = Path.parseS(spathS, NamespaceMap(mmt.mmtbase))
      val constants = controller.get(spath) match {
        case c: Constant => c.not match {
          case None => Nil
          case Some(a) => List(c.name -> a.fixity.markers)
        }
        case _ => Nil
      }

      def modifyStringRepresent(name: LocalName, markers: List[Marker], accumulator: String, hasSymbol: Boolean): (String) = {
        val separator = " "
        markers match {
          case Nil =>
            if (hasSymbol) accumulator
            else name.toString + accumulator
          case hd :: b => if (hd.toString == "%n") {
            modifyStringRepresent(name, b, accumulator, false)
          } else if (hd.toString.length() > 1 && hd.toString.substring(0, 2) == "%I")
            modifyStringRepresent(name, b, accumulator, hasSymbol)
          else modifyStringRepresent(name, b, accumulator + separator + hd.toString, hasSymbol)
        }
      }
      constants.map({ case (x, y) => modifyStringRepresent(x, y, "", true) })
    } catch {
      case e: Throwable => Nil
    }
  }

  def getTrefiCompletion(spathS: String): List[String] = {
    try {
      val spath = Path.parseS(spathS, NamespaceMap(mmt.mmtbase))
      val constants = controller.get(spath) match {
        case c: Constant => c.notC.getVerbal match {
          case None => Nil
          case Some(a) => List(c.name -> a.fixity.markers)
        }
        case _ => Nil
      }

      def modifyStringRepresent(name: LocalName, markers: List[Marker], accumulator: String, hasSymbol: Boolean): (String) = {
        val separator = " "
        markers match {
          case Nil =>
            if (hasSymbol) accumulator
            else name.toString + accumulator
          case hd :: b => if (hd.toString == "%n") {
            modifyStringRepresent(name, b, accumulator, false)
          } else if (hd.toString.length() > 1 && hd.toString.substring(0, 2) == "%I")
            modifyStringRepresent(name, b, accumulator, hasSymbol)
          else modifyStringRepresent(name, b, accumulator + separator + hd.toString, hasSymbol)
        }
      }
      constants.map({ case (x, y) => modifyStringRepresent(x, y, "", true) })
    } catch {
      case e: Throwable => Nil
    }
  }

  /*returns the distance between two words
   * distance: the minimum number of operations(substitutions/deletions/insertions) to transform one word to another
   */
  private def WordDistance(first: String, second: String): Int = {
    val m = first.length()
    val n = second.length()

    val distance = Array.ofDim[Int](m + 1, n + 1)
    var i = 0
    var j = 0
    for (i <- 1 to m)
      distance(i)(0) = i
    for (j <- 1 to n)
      distance(0)(j) = j

    for (j <- 1 to n)
      for (i <- 1 to m) {
        if (first.charAt(i - 1).equals(second.charAt(j - 1)))
          distance(i)(j) = distance(i - 1)(j - 1) // no operation required
        else
          distance(i)(j) = Math.min(Math.min(
            distance(i - 1)(j) + 1, // a deletion
            distance(i)(j - 1) + 1), // an insertion
            distance(i - 1)(j - 1) + 1 // a substitution
          )
      }
    distance(m)(n)
  }


  private def modifyStringRepresent(name: LocalName, markers: List[Marker], accumulator: String, hasSymbol: Boolean): String = {
    val separator = " "
    markers match {
      case Nil =>
        if (hasSymbol) accumulator
        else name.toString + accumulator
      case hd :: b => if (hd.toString == "%n") {
        modifyStringRepresent(name, b, accumulator, false)
      } else if (hd.toString.length() > 1 && hd.toString.substring(0, 2) == "%I")
        modifyStringRepresent(name, b, accumulator, hasSymbol)
      else modifyStringRepresent(name, b, accumulator + separator + hd.toString, hasSymbol)
    }
  }


  private def closestWord(word: String, possibilites: List[String], firstN: Int = 1): String = {
    val distances = possibilites.map(x => WordDistance(word, x))
    val min = distances.zipWithIndex.min
    val index = min._2
    val element = min._1
    possibilites(index)
  }


  /*checks if there is a similar constant in the includes recursively
   * resolveIncludes should be called first, if not resolved, one can try this
   * */
  def getConstantCorrection(request: MMTConstantCorrectionRequest): MMTConstantCorrectionResponse = {
    val mpath = Path.parse(request.getMPath(), NamespaceMap(mmt.mmtbase))
    val constant = request.getConstant()

    def searchOnIncludes(mpath: Path): List[(String, Int)] = {
      val declarations = controller.get(mpath) match {
        case t: DeclaredTheory => t
        case _ => throw new ServerError("No declarations")
      }
      val includes = declarations.getIncludes

      if (includes == Nil) {
        return List[(String, Int)]()
      }

      val constants = controller.get(mpath) match {
        case t: DeclaredTheory => t.getConstants.map(_.name.toString)
        case _ => Nil
      }

      val possibleMatches = constants

      val answer = possibleMatches.map(x => x -> WordDistance(x, constant)).minBy(_._2)

      includes.foldRight(List(answer))((b, a) => searchOnIncludes(b) ::: List(a.minBy(_._2)))
    }

    val response = searchOnIncludes(mpath).minBy(_._2)

    new MMTConstantCorrectionResponse(response._1)
  }

  //a function to get the closest match of an include which is not written properly
  def getIncludeCorrection(request: MMTIncludeCorrectionRequest)(implicit lib: Library): MMTIncludeCorrectionResponse = {
    val allPaths = controller.depstore.getInds(ontology.IsTheory).toList //lib.getAllPaths.toList
    if (allPaths == Nil) {
      return new MMTIncludeCorrectionResponse("")
    }
    val term = request.getTerm()
    val possibleMatches = allPaths.map(_.toString)

    val response = closestWord(term, possibleMatches)


    new MMTIncludeCorrectionResponse(response)
  }


  def getSymbolDefinitions(spathS: String): Map[String, (String, String)] = {
    val res = new collection.mutable.HashMap[String, (String, String)]
    try {
      val spath = Path.parseS(spathS, NamespaceMap(mmt.mmtbase))
      controller.get(spath) match {
        case c: Constant =>
          val verbs = c.notC.verbalizationDim.notations.values.flatten.toList
          val defs = controller.depstore.queryList(spath, ontology.ToObject(informal.IRels.isDefinedBy))
          defs foreach {
            case p: GlobalName =>
              //getting language
              //FIXME terrible hack, to add language field to definition instead
              val lang = p.module.name.toPath.split('.').toList match {
                case l if l.contains("de") => "de"
                case l if l.contains("en") => "en"
                case l => throw new IllegalStateException
              }
              val verbalization = verbs.find(_.scope.languages.contains(lang)).map(n => modifyStringRepresent(p.name, n.fixity.markers, "", true)).getOrElse("")
              res(lang) = (p.toPath -> verbalization)
            case _ => throw new IllegalStateException
          }
          res.toMap
        case _ => res.toMap
      }
    } catch {
      case e: Exception => res.toMap
      case e: Error => res.toMap
    }
  }

}
