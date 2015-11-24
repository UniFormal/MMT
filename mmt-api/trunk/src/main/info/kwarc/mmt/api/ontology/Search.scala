package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import objects._
import parser._
import notations._
import frontend._

/** regular expressions to be matched against components of a [[Path]] */
case class PathPattern(basePattern: Option[String], modulePattern: Option[String], namePattern: Option[String]) {
   def matches(path: Path): Boolean = {
      val p = path match {
         case c: CPath => c.parent
         case p => p
      }
      val (modOpt, nameOpt) = p match {
         case _: DPath => (None, None)
         case m: MPath => (Some(m.name), None)
         case g: GlobalName => (Some(g.module.name), Some(g.name))
         case _ => throw ImplementationError("")
      }
      val comps = Some(p.doc.toPath) :: List(modOpt, nameOpt).map(c => c.map(_.toPath))
      (List(basePattern, modulePattern, namePattern) zip comps).forall {
         case (Some(p), Some(s)) => s.matches(p)
         case (Some(p), None) => false
         case (None, _) => true
      }
   }
}

/** a pattern to be matched against a collection of terms, e.g., via [[MathWebSearch]] */
case class TermPattern(qvars: Context, query: Term)

object TermPattern {
   val qvarBinder = utils.mmt.mmtcd ? "qvar"
   val qvarMarkers = List(Delim("$"), Var(1, false, Some(Delim(","))), Delim(":"), Arg(2))
   val qvarNot = new TextNotation(Mixfix(qvarMarkers), Precedence.infinite, None)
   val qvarRule = ParsingRule(qvarBinder, qvarNot)
   class RemoveUnknowns(unk: Context) extends StatelessTraverser {
      def traverse(t: Term)(implicit con : Context, init : State) = t match {
         case OMA(OMS(_), OMV(x) :: _) if unk.isDeclared(x) && ! con.isDeclared(x) => OMV(x)
         case _ => Traverser(this, t)
      }
   }
   /** parses $ qvars:query, all unknown variables are turned into additional query variables */
   def parse(controller: Controller, theory: String, pattern: String, format: String): TermPattern = {
      val mp = Path.parseM(theory, NamespaceMap(utils.mmt.mmtcd))
      val pu = ParsingUnit(SourceRef.anonymous(pattern), Context(mp), pattern, NamespaceMap(mp), Some(qvarRule))
      val parser = controller.extman.get(classOf[ObjectParser], format).getOrElse(throw ParseError("no parser found for $format"))
      val unkQP = parser(pu)(ErrorThrower)
      val (unk, qP) = ObjectParser.splitOffUnknowns(unkQP)
      val qPM = (new RemoveUnknowns(unk)).apply(qP, Context())
      qPM match {
         case OMBIND(OMS(this.qvarBinder), qvars, qBody) => TermPattern(qvars++unk, qBody)
         case t => TermPattern(unk, t)
      }
   }
}

/** a query that can be sent to [[Search]] */
case class SearchQuery(pp: PathPattern, comps: List[ComponentKey], pattern: Option[TermPattern])
/** a result returned by [[Search]] or [[MathWebSearch]] */
case class SearchResult(cpath: CPath, pos: Position, term: Option[Term])

/** an implementation of a search algorithm that calls [[MathWebSearch]] and filters the results */
class Search(controller: Controller) {
   def apply(sq: SearchQuery, resolveResults: Boolean): List[SearchResult] = {
      val mws = controller.extman.mws.getOrElse(throw ParseError("no mws defined"))
      val results = sq.pattern match {
         case Some(pattern) =>
            val mwsResults = mws(MathWebSearchQuery(pattern, 10))
            mwsResults.flatMap {case r @ SearchResult(cp, pos, _) =>
               if (sq.pp.matches(cp.parent) && sq.comps.contains(cp.component))
                  List(r)
               else
                  Nil
            }
         case None => Nil
      }
      if (resolveResults) {results.map {case SearchResult(cp, pos, _) =>
         val term = controller.globalLookup.getComponent(cp).asInstanceOf[AbstractTermContainer].get
         SearchResult(cp, pos, term)
      }} else results
   }
}