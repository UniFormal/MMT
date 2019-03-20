package info.kwarc.mmt.intellij

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.MathHub
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend.{Controller, MMTConfig, ReportHandler}
import info.kwarc.mmt.api.gui._
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{Context, Obj, Term, VarDecl}
import info.kwarc.mmt.api.parser.SourceRegion
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.refactoring.linkinversion._
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem}
import info.kwarc.mmt.intellij.checking.{Checker, ErrorViewer}
import info.kwarc.mmt.refactoring.linkinversion.LFLinkInverter
import javax.swing.tree.DefaultMutableTreeNode

import scala.language.reflectiveCalls
import scala.util.Try

class MMTPluginInterface(homestr: String, reportF: Any) {
	val home = File(homestr)
	private var controller: Controller = _

	lazy val errorReport = new ReportHandler("IntelliJ") {
		private val report = reportF.asInstanceOf[ {
			def apply(s: String): Unit
		}]

		override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = if (group == "error") report(msgParts.mkString(" "))

	}

	private object Abbreviations {
		lazy val pairstrings = MMTSystem.getResourceAsString("latex/unicode-latex-map").split("\n")
		lazy val pairs: List[(String, String)] = pairstrings.collect { case s if s.nonEmpty =>
			val ls = s.split('|')
			if (ls.length != 2) {
				println(ls.mkString(", "))
				???
			}
			( /*"""\""" + */ ls.head.trim /*.drop(1)*/ , ls.last.trim)
		}.filterNot(p => List("❚", "❙", "❘").contains(p._2)).toList
	}

	private object LocalMathHub {
		lazy val mathhub = new MathHub(controller, home, MathHub.defaultURL, true) {
			lazy val all = available_()
		}

		def remotes = mathhub.all.filter(id => !localArchs.exists(_._1 == id))

		def archives = controller.backend.getArchives.map(_.id)

		def localGroups = archives.map { id =>
			if (id.contains("/")) id.split('/').head
			else "Others"
		}.distinct

		def remoteGroups = mathhub.all.collect {
			case id if id.contains("/") =>
				id.split('/').head
		}.distinct.filter(!localGroups.contains(_))

		def localArchs = archives.map { id =>
			(id,
				if (id.contains("/")) id.split('/').mkString(".")
				else "Others." + id)
		}
	}

	def archiveInfo(id: String) = {
		val archive = controller.backend.getArchive(id).get
		val meta_inf = archive.root / "META-INF"
		val source = archive / archives.source
		val scala = archive.root / "scala" // TODO redirectable dimension, if existent
		(source.toString, meta_inf.toString, scala.toString)
	}

	def localGroups() = LocalMathHub.localGroups.toArray

	def remoteGroups() = LocalMathHub.remoteGroups.toArray

	def localArchs() = LocalMathHub.localArchs.toArray

	def remoteArchs() = LocalMathHub.remotes.toArray

	def version() = controller.getVersion

	private lazy val ev = new ErrorViewer(controller)

	def errorViewer() = ev

	private lazy val check = new Checker(controller, ev)

	def checker() = check

	def shell(_doLine: Any) = {
		val doLine = _doLine.asInstanceOf[ {
			def apply(v1: String): Unit
		}]
		val handler = new ReportHandler("IntelliJ Shell") {
			override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
				msgParts.foreach { msg => doLine(indentString(ind) + group + ": " + msg) }
			}
		}
		controller.report.addHandler(handler)
	}

	def handleLine(s: String) = controller.handleLine(s)

	def abbrevs(): Array[(String, String)] = Abbreviations.pairs.toArray

	def clear(): Unit = controller.clear

	def getArchiveRoots() = LocalMathHub.archives.map { id =>
		controller.backend.getArchive(id).get.root.toString
	}.toArray

	def install(id: String) = {
		LocalMathHub.mathhub.installEntry(id, None, true)
		while (controller.backend.getArchive(id).isEmpty) Thread.sleep(100)
	}

	// private val psithings : mutable.HashMap[Any,(DPath,NamespaceMap)] = mutable.HashMap.empty
	def parseNamespace(s: String): String = Path.parseD(s, controller.getNamespaceMap).toString

	def getNamespaceFromFile(filestr: String): String = {
		val file = File(filestr)
		controller.backend.resolvePhysical(file) match {
			case Some((a, _)) => a.ns match {
				case Some(d: DPath) => return d.toString
				case _ =>
			}
			case _ =>
		}
		controller.getNamespaceMap.base.doc.toString
	}

	def getNamespaceMap(ns: String, pars: List[(String, String)]) = {
		val dp = Path.parseD(ns, controller.getNamespaceMap)
		var nsm = controller.getNamespaceMap(dp)
		pars foreach { case (a, p) =>
			nsm = nsm.add(a, Path.parse(p, nsm).toPath)
		}
		nsm
	}

	def resolvePathSimple(str: String, nsm: AnyRef) = {
		val nsMap = nsm.asInstanceOf[NamespaceMap]
		Path.parse(str, nsMap).toString
	}

	def resolvePath(wordS: String, nsm: AnyRef) = { // TODO from [[NotationBasedParser.makeIdentifier]]
		var word = wordS
		val nsMap = nsm.asInstanceOf[NamespaceMap]
		val segments = utils.stringToList(word, "\\?")
		// recognizing identifiers ?THY?SYM is awkward because it would require always lexing initial ? as identifiers
		// but we cannot always prepend ? because the identifier could also be NS?THY
		// Therefore, we turn word into ?word using a heuristic
		segments match {
			case fst :: _ :: Nil if !fst.contains(':') && fst != "" && Character.isUpperCase(fst.charAt(0)) =>
				word = "?" + word
			case _ =>
		}
		// recognizing prefix:REST is awkward because : is usually used in notations
		// therefore, we turn prefix/REST into prefix:/REST if prefix is a known namespace prefix
		// this introduces the (less awkward problem) that relative paths may not start with a namespace prefix
		val beforeFirstSlash = segments.headOption.getOrElse(word).takeWhile(_ != '/')
		if (!beforeFirstSlash.contains(':') && nsMap.get(beforeFirstSlash).isDefined) {
			word = beforeFirstSlash + ":" + word.substring(beforeFirstSlash.length)
		}
		try {
			Path.parse(word, nsMap).toString
		} catch {
			case ParseError(msg) =>
				null
		}
	}

	def getReference(uri: String) = Path.parse(uri) match {
		case mp: MPath if mp.parent.uri.scheme contains "scala" =>
			Some(SemanticObject.mmtToJava(mp, true))
		case _ => None
	}

	def init(): Unit = {
		controller = new Controller
		controller.report.addHandler(errorReport)

		/** Options */
		val mslf = home / "startup.msl"
		if (mslf.toJava.exists())
			controller.runMSLFile(mslf, None)
		else {
			mslf.createNewFile()
			File.append(mslf, "extension info.kwarc.mmt.odk.Plugin")
		}

		val rc = home / "mmtrc"
		if (!rc.toJava.exists()) {
			rc.createNewFile()
			File.append(rc, "\n", "#backends\n", "lmh .")
		}

		controller.loadConfig(MMTConfig.parse(rc), false)

		/** MathHub Folder */
		controller.setHome(home)
		controller.addArchive(home)
	}

	def syntaxTree(node: DefaultMutableTreeNode, docS: String): Unit = {
		val dp = Path.parseD(docS, NamespaceMap.empty)
		val doc = Try(controller.getDocument(dp)).getOrElse {
			return ()
		}
		TreeBuilder.buildTreeDoc(node, doc)
	}

	private object TreeBuilder extends NavigationTreeBuilder(controller) {

		abstract class Ret(reg: Option[SourceRegion]) extends {
			protected val label: String

			override def toString: String = label

			def getOffset: Int = reg.getOrElse(return 0).start.offset

			def getEnd: Int = reg.getOrElse(return 0).end.offset
		}

		case class Doc(elem: Document, region: SourceRegion) extends Ret(Some(region)) with MMTElemAsset {
			val label = elem.name.last.toString
		}

		case class Mod(elem: modules.Module, region: SourceRegion) extends Ret(Some(region)) with MMTElemAsset {
			val label = elem.name.toString
		}

		case class Dec(elem: Declaration, region: SourceRegion) extends Ret(Some(region)) with MMTElemAsset {
			val label = elem.name match {
				case LocalName(ComplexStep(mp) :: Nil) => "include ?" + mp.name
				case _ => elem.name.toString
			}
		}

		val ctrl = controller

		case class Ob(label: String, region: SourceRegion, parent: CPath, context: Context, obj: Obj, pragmatic: Obj)
			extends Ret(Some(region)) with MMTObjAsset {
			val controller = ctrl
		}

		case class Not(owner: ContentPath, cont: NotationContainer, comp: NotationComponentKey, region: SourceRegion) extends Ret(Some(region)) with MMTNotAsset {
			val not = cont(comp).get
			val label = not.toString
		}

		case class Text(label: String) extends Ret(None) with MMTAuxAsset

		case class Uri(path: Path, region: SourceRegion) extends Ret(Some(region)) with MMTURIAsset

		override def makeDocument(doc: Document, region: SourceRegion) = Doc(doc, region)

		override def makeModule(mod: modules.Module, region: SourceRegion) = Mod(mod, region)

		override def makeDeclaration(dec: Declaration, region: SourceRegion) = Dec(dec, region)

		override def makeComponent(t: Term, cont: Context, parent: CPath, region: SourceRegion)
		= Ob(parent.component.toString, region, parent, cont, t, t)

		//Ret(parent.component.toString,Some(region))
		override def makeTerm(t: Term, pragmatic: Term, cont: Context, parent: CPath, label: String, region: SourceRegion)
		= Ob(label, region, parent, cont, t, pragmatic)

		//= Ret(label,Some(region))
		override def makeNotation(owner: ContentPath, cont: NotationContainer, comp: NotationComponentKey, region: SourceRegion)
		= Not(owner, cont, comp, region)

		override def makeSection(s: String) = Text(s)

		override def makeNRef(uri: Path, region: SourceRegion) = Uri(uri, region)

		override def makeVariableInContext(con: Context, vd: VarDecl, parent: CPath, region: SourceRegion)
		= Ob(vd.name.toString, region, parent, con, vd, vd)

		//Ret(vd.name.toString,Some(region))
	}

	def buildFile(f: String) {
		val file = File(f)
		val errorCont = new ErrorHandler {
			override protected def addError(e: Error): Unit = {}
		}
		if (file.isFile) {
			try {
				controller.build(file)(errorCont)
			} catch {
				case e: Error => errorCont(e)
			}
		}
	}

	def checkUpdate: Option[(String, String)] = Try(MMTSystem.getLatestVersion).toOption

	private var hasReadAllRelationalContent = false

	private def readAllRelationalContent(): Unit = {
		if (!hasReadAllRelationalContent) {
			controller.backend.getArchives.foreach(archive => {
				archive.allContent
				archive.readRelational(FilePath("/"), controller, "rel")
			})

			hasReadAllRelationalContent = true
		}
	}

	/* Generalizer Tool */
	def generalize(
									errorTreeRootNode: DefaultMutableTreeNode,
									RPath: String,
									SPath: String,
									RToSPath: String,
									TPath: String
								): String = {

		def getMMTSyntaxPresenterExtension: MMTSyntaxPresenter = {
			controller.extman.get(classOf[MMTSyntaxPresenter], format = "").getOrElse {
				val firstInstance = new MMTSyntaxPresenter()
				controller.extman.addExtension(firstInstance)

				firstInstance
			}
		}

		// Precondition: StructuralElement added to controller
		val elementToMMTSurfaceCode: StructuralElement => String = {
			val mmtSyntaxPresenter = getMMTSyntaxPresenterExtension

			element: StructuralElement => {
				val stringBuilder = new presentation.StringBuilder
				mmtSyntaxPresenter(element)(stringBuilder)

				stringBuilder.get
			}
		}

		object ErrorTreeExtendingErrorHandler extends RewriteErrorHandler {
			def apply(error: RewriteError): ContinuationStyle = error match {

				case RewriteConstantError(originalDecl, attemptedDecl, blamableTerms) =>

					val declErrorNode = new DefaultMutableTreeNode(
						"?" + originalDecl.name.toStr(shortURIs = true)
					)

					blamableTerms.collect {
						case (componentKey, failingTerms) if failingTerms.nonEmpty =>
							val componentKeyErrorNode = new DefaultMutableTreeNode(
								componentKey.toString
							)

							failingTerms.map(failingTerm => {
								// TODO(ComFreek) Currently failing terms are always (?) OMIDs
								//   but might not be in the future. Maybe then use
								//   [[NotationBasedPresenter]]?
								val failingTermErrorNode = new DefaultMutableTreeNode(
									failingTerm.toStr(shortURIs = true)
								)

								failingTermErrorNode
							}).foreach(componentKeyErrorNode.add)

							componentKeyErrorNode
					}.foreach(declErrorNode.add)

					errorTreeRootNode.add(declErrorNode)

					SkipDeclaration

				case RewriteUnknownError(originalDecl) =>
					val unknownErrorNode = new DefaultMutableTreeNode(
						"Inversion not implemented for the declaration type used in " +
							originalDecl.toString
					)
					errorTreeRootNode.add(unknownErrorNode)

					SkipDeclaration
			}
		}

		val nsMap = NamespaceMap.empty

		val R = controller.getTheory(Path.parseM(RPath, nsMap))
		val S = controller.getTheory(Path.parseM(SPath, nsMap))
		val T = controller.getTheory(Path.parseM(TPath, nsMap))
		val RToS = controller.getAs(classOf[View], Path.parseM(RToSPath, nsMap))

		val newModulePath = T.path.parent ? (T.path.name + "Generalized")
		val generatedMorphismPath = T.path.parent ? (T.path.name + "GeneralizedMorphism")

		// [[LinkInverter]] needs all relational data
		readAllRelationalContent()

		val (invertedTheory, generatedMorphism) = LFLinkInverter.invertLink(
			R,
			T,
			RToS,
			newModulePath,
			generatedMorphismPath,
			ErrorTreeExtendingErrorHandler
		)(controller)


		controller.add(invertedTheory)
		controller.add(generatedMorphism)
		val invertedTheoryCode = elementToMMTSurfaceCode(invertedTheory)
		val generatedMorphismCode = elementToMMTSurfaceCode(generatedMorphism)
		controller.delete(invertedTheory.path)

		invertedTheoryCode + "\n\n" + generatedMorphismCode
	}
}
