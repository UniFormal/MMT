package info.kwarc.mmt.marpa

import scala.Option.option2Iterable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.matching.Regex
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONObject
import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.Error
import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.SourceError
import info.kwarc.mmt.api.StructuralElement
import info.kwarc.mmt.api.backend.XMLReader
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Logger
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.notations.Arg
import info.kwarc.mmt.api.notations.Delimiter
import info.kwarc.mmt.api.notations.GroupMarker
import info.kwarc.mmt.api.notations.ImplicitArg
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.notations.SeqArg
import info.kwarc.mmt.api.notations.TdMarker
import info.kwarc.mmt.api.notations.TextNotation
import info.kwarc.mmt.api.notations.PlaceholderDelimiter
import info.kwarc.mmt.api.notations.SymbolName

import info.kwarc.mmt.api.notations.InstanceName

import info.kwarc.mmt.api.notations.Var
import info.kwarc.mmt.api.ontology.Binary
import info.kwarc.mmt.api.informal.IRels._
import info.kwarc.mmt.api.parser
import info.kwarc.mmt.api.presentation.StringBuilder
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.web.Body
import info.kwarc.mmt.api.web.Server
import info.kwarc.mmt.api.web.ServerError
import info.kwarc.mmt.api.web.ServerExtension
import info.kwarc.mmt.stex.STeXImporter
import info.kwarc.mmt.stex.sTeX
import tiscaf.HLet
import info.kwarc.mmt.api._
import tiscaf.HTalk
import info.kwarc.mmt.api.objects._

abstract class GenericRule
case class Rule(name: String, content: List[String]) extends GenericRule

object Grammar {
  //TODO: Extract precedences from notations instead of hard coding
  val precedences = List("-infinity", "0", "200", "300", "400", "500", "550", "600", "700", "800", "900")
  //TODO: rules that start or end with the following are not considered
  private var omittedRules = Set("N482", "_coset_", "_polynomialring_", "_logarithm_logarithm");

  var currentTopRuleNr = ""
  var currentTopRulePrec = 1

  var rules = Set.empty[Rule]
  private var NotationContent = List.empty[String]
  private var eventList = List.empty[String];
  private var index: Int = 0 //used to ensure the uniqueness of rule names

  //Top Rule Name uniqueness cannot be assumed because the path of the notation
  //can be common for multiple notations - hence "_" is added to differentiate between those
  def createTopRuleName(name: String): String = {
    if (rules.exists(x ⇒ x match { case Rule(n, c) ⇒ n == name })) {
      createTopRuleName("_" + name)
    } else {
      name
    }
  }

  private def isOmitted(rule: String): Boolean = {
    omittedRules.foldLeft(false) { (acc: Boolean, omitted: String) ⇒
      acc || rule.startsWith(omitted) || rule.endsWith(omitted)
    }
  }

  //adds the Top Rule and all of the necessary subrules to the Grammar
  def addTopRule(path: String, ruleNumber: String, markers: List[Marker], prec: Precedence) {
    val notNamePattern = """\?.*""".r
    val removeInvalidCharPattern = """[^a-zA-Z0-9]""".r
    val Some(notNameWithInvalidChars) = notNamePattern findFirstIn path
    val notName = removeInvalidCharPattern.replaceAllIn(notNameWithInvalidChars, "_")
    val precStr = precedences.indexOf(prec.toString).toString
    val uniqueName = createTopRuleName(notName + "P" + precStr + "N" + ruleNumber)

    NotationContent ::= uniqueName
    currentTopRuleNr = ruleNumber
    currentTopRulePrec = precStr.toInt

    val content: List[String] = markers.map(
      x ⇒ x match {
        //	          NOTE: #seq_ is used to indicate whether the Delim is 
        //	          inside a sequence argument, since if it is in one
        //	          the text inside the Delim should never be empty
        //	          to avoid infinite left recursion
        case Delim(text) ⇒ addRule(Delim("#seq_" + text))
        case _           ⇒ addRule(x)
      })

    if (!(content.length == 1
      && content(0).startsWith("argRule")) // do not include rules that only have one argRule (creates cycles)	   
      ) {
      rules = rules | Set((Rule(uniqueName, "topLevel" :: content))) //uniqueness of top level notations is assumed
    } else {
      omittedRules += uniqueName
    }
    if (!isOmitted(uniqueName)) {
      eventList ::= "event '" + uniqueName + "' = completed " + uniqueName
    }
  }

  //returns the name of the rule, adds the rule to the Set[Rule] only if it is not already there
  def createRule(content: List[String]): String = {
    val filteredRules = rules.filter(r ⇒ r match { case Rule(n, c) ⇒ c == content })
    if (filteredRules.isEmpty) {
      index = index + 1
      val name = "rule" + index.toString
      rules = rules | Set(Rule(name, content))
      name
    } else {
      val Rule(name, _) = filteredRules.head
      name
    }
  }

  def createArgRule(topRuleNr: String, suff: String, precTmp: Int = -1): String = {
    val prec = if (precTmp == -1) currentTopRulePrec else precTmp
    if (prec != currentTopRulePrec) {
      println("Arg rule with precedence = " + prec.toString) //argument rules with custom precedences were used
    }
    val name = "argRuleN" + topRuleNr + "A" + suff
    val filteredRules = rules.filter(r ⇒ r match { case Rule(n, c) ⇒ n == name })
    if (filteredRules.isEmpty) {
      rules = rules | Set(Rule(name, "renderB" :: prec.toString :: Nil))
      name
    } else {
      name
    }
  }

  //returns the name of the rule, adds the rule to the grammar by calling CreateRule
  //this function performs the recursion on Markers, while CreateRule just checks for uniqueness
  def addRule(marker: Marker): String = marker match {

    case SimpArg(argNr, precedence) ⇒ {
      precedence match {
        case Some(prec) ⇒
          createArgRule(currentTopRuleNr, argNr.toString + "Arg", precedences.indexOf(prec.toString))
        case None ⇒
          createArgRule(currentTopRuleNr, argNr.toString + "Arg")
      }
    }

    case Var(argNr, false, None, precedence) ⇒ {
      precedence match {
        case Some(prec) ⇒
          createArgRule(currentTopRuleNr, argNr.toString + "Var", precedences.indexOf(prec.toString))
        case None ⇒
          createArgRule(currentTopRuleNr, argNr.toString + "Var")
      }
    }

    case Var(argNr, false, Some(delim), precedence) ⇒ {
      val Delim(text) = delim
      val delimName = addRule(Delim("#seq_" + text))
      val content = precedence match {
        case Some(x) ⇒
          val argName: String = createArgRule(currentTopRuleNr, argNr.toString + "VarSeq", precedences.indexOf(x.toString))
          "iteratevarB" :: "nrB" :: argNr.toString :: "nrE" :: "prB" :: x.toString :: "prE" ::
            "separatorB" :: delimName :: "separatorE" :: argName :: "iteratevarE" :: Nil
        case None ⇒
          val argName: String = createArgRule(currentTopRuleNr, argNr.toString + "VarSeq")
          "iteratevarB" :: "nrB" :: argNr.toString :: "nrE" :: "separatorB" :: delimName ::
            "separatorE" :: argName :: "iteratevarE" :: Nil
      }
      val result = createRule(content)
      result
    }

    case SimpSeqArg(argNr, delim, precedence) ⇒ {
      val Delim(text) = delim
      val delimName = addRule(Delim("#seq_" + text))
      val content = precedence match {
        case Some(x) ⇒
          val argName: String = createArgRule(currentTopRuleNr, argNr.toString + "ArgSeq", precedences.indexOf(x.toString))
          "iterateB" :: "nrB" :: argNr.toString :: "nrE" :: "prB" :: x.toString :: "prE" ::
            "separatorB" :: delimName :: "separatorE" :: argName :: "iterateE" :: Nil
        case None ⇒
          val argName: String = createArgRule(currentTopRuleNr, argNr.toString + "ArgSeq")
          "iterateB" :: "nrB" :: argNr.toString :: "nrE" :: "separatorB" :: delimName ::
            "separatorE" :: argName :: "iterateE" :: Nil
      }
      val result = createRule(content)
      result
    }
    case m: TdMarker ⇒
      val content = m.content map addRule
      if (content.isEmpty) {
        createRule("mtdSingle" :: Nil)
      } else {
        val contentRule = createRule("contentRule" :: content)
        createRule("mtdB" :: contentRule :: "mtdE" :: Nil)
      }

    case m: TrMarker ⇒
      val content = m.content map addRule
      if (content.isEmpty) {
        createRule("mtrSingle" :: Nil)
      } else {
        val contentRule = createRule("contentRule" :: content)
        createRule("mtrB" :: contentRule :: "mtrE" :: Nil)
      }
    case m: TableMarker ⇒
      val content = m.content map addRule
      if (content.isEmpty) {
        createRule("mtableSingle" :: Nil)
      } else {
        val contentRule = createRule("contentRule" :: content)
        createRule("mtableB" :: contentRule :: "mtableE" :: Nil)
      }
    //////////////////////////////////////////////////////////////
    case NumberMarker(d) ⇒
      var text = d.text
      var flag = ""
      text = if (text.startsWith("#seq_")) {
        flag = "#seq_"
        text.substring(5)
      } else {
        text
      }
      if (text == "") {
        text = " "
      }

      if (text == "⁢" //unicode 
        && flag == "") { //handling the invisible unicode char used for function application
        val v1 = createRule("empty" :: Nil)
        val v2 = createRule("mnB" :: text.toString :: "mnE" :: Nil)
        createRule("alternatives" :: v1 :: v2 :: Nil)
      } else {
        createRule("mnB" :: text.toString :: "mnE" :: Nil)
      }
    case Delim(rawW) ⇒
      //first check if this Delim is from a Sequence Argument - if so the rule
      //cannot be nullable
      var flag = ""
      var w = if (rawW.startsWith("#seq_")) {
        flag = "#seq_"
        rawW.substring(5)
      } else {
        rawW
      }
      if (w == "&#40;") { w = "(" }
      if (w == "&#41;") { w = ")" }
      if (w.startsWith("#num_")) {
        addRule(NumberMarker(Delim(flag + w.substring(5))))
      } else if (w.startsWith("#id_")) {
        addRule(IdenMarker(Delim(flag + w.substring(4))))
      } else {
        var text = w
        if (text == "") {
          text = " "

        }
        if ((text == """⁢""" || text == "⁡") //unicode
          && flag == "") { //handling the invisible unicode char used for function application
          val v1 = createRule("empty" :: Nil)
          val v2 = createRule("moB" :: text.toString :: "moE" :: Nil)
          createRule("alternatives" :: v1 :: v2 :: Nil)
        } else {
          createRule("moB" :: text.toString :: "moE" :: Nil)
        }
      }
    case IdenMarker(d) ⇒
      var text = d.text
      var flag = ""
      text = if (text.startsWith("#seq_")) {
        flag = "#seq_"
        text.substring(5)
      } else {
        text
      }
      if (text == "") {
        text = " "
      }
      if (text == """⁢""" //unicode
        && flag == "") { //handling the invisible unicode char used for function application
        val v1 = createRule("empty" :: Nil)
        val v2 = createRule("miB" :: text.toString :: "miE" :: Nil)
        createRule("alternatives" :: v1 :: v2 :: Nil)
      } else {
        createRule("miB" :: text.toString :: "miE" :: Nil)
      }
    /////////////////////////////////////////////////////
    case m: GroupMarker ⇒
      val content: List[String] = m.elements.map(addRule)
      if (content.isEmpty) {
        createRule("mrowSingle" :: Nil)
      } else {
        val result = createRule("Group" :: content)
        result
      }

    case f: FractionMarker ⇒
      val above = addRule(GroupMarker(f.above))
      val below = addRule(GroupMarker(f.below))
      createRule(List("mfracB", above, below, "mfracE"))
    case RootMarker(baseMarkerList, indexMarkerList) ⇒
      val base = addRule(GroupMarker(baseMarkerList))
      if (indexMarkerList == Nil) {
        createRule("msqrtB" :: base :: "msqrtE" :: Nil)
      } else {
        val index = addRule(GroupMarker(indexMarkerList))
        createRule("mrootB" :: base :: index :: "mrootE" :: Nil)
      }
    case s: ScriptMarker ⇒
      val mainRule = addRule(s.main)
      var hasSup = false
      var hasSub = false
      var hasUnder = false
      var hasOver = false
      val subRule = s.sub match {
        case Some(m) ⇒ {
          hasSub = true
          addRule(m)
        }
        case _ ⇒ "none_sub"
      }
      val supRule = s.sup match {
        case Some(m) ⇒ {
          hasSup = true
          addRule(m)
        }
        case _ ⇒ "none_sup"

      }

      val overRule = s.over match {
        case Some(m) ⇒ {
          hasOver = true
          addRule(m)
        }
        case _ ⇒ "none_over"
      }

      val underRule = s.under match {
        case Some(m) ⇒ {
          hasUnder = true
          addRule(m)
        }
        case _ ⇒ "none_under"
      }
      val result = if (hasSup && !hasSub) {
        createRule("msupB" :: mainRule :: supRule :: "msupE" :: Nil)
      } else if (!hasSup && hasSub) {
        createRule("msubB" :: mainRule :: subRule :: "msubE" :: Nil)
      } else if (hasSup && hasSub) {
        val v1 = createRule("msubB" ::
          "msupB" :: mainRule :: supRule :: "msupE" ::
          subRule :: "msubE" :: Nil)
        val v2 = createRule("msupB" ::
          "msubB" :: mainRule :: subRule :: "msubE" ::
          supRule :: "msupE" :: Nil)
        val v3 = createRule("msubsupB" ::
          mainRule :: subRule :: supRule :: "msubsupE" :: Nil)
        createRule("alternatives" :: v1 :: v2 :: v3 :: Nil)

      } else if (hasOver && !hasUnder) {
        createRule("moverB" :: mainRule :: overRule :: "moverE" :: Nil)
      } else if (!hasOver && hasUnder) {
        createRule("munderB" :: mainRule :: underRule :: "munderE" :: Nil)
      } else if (hasOver && hasUnder) {
        val v1 = createRule("munderB" ::
          "moverB" :: mainRule :: overRule :: "moverE" ::
          underRule :: "munderE" :: Nil)
        val v2 = createRule("moverB" ::
          "munderB" :: mainRule :: underRule :: "munderE" ::
          overRule :: "moverE" :: Nil)
        val v3 = createRule(
          "munderoverB" :: mainRule :: underRule :: overRule :: "munderoverE" :: Nil)
        createRule("alternatives" :: v1 :: v2 :: v3 :: Nil)

      } else if (!hasSub && !hasSup && !hasUnder && !hasOver) {
        mainRule
      } else {
        "SCRIPTMARKER_FAIL"
      }

      result
    //	   case Subs(nr,precOp) =>  addRule(Arg(nr,precOp))
    //		   case SqrtMarker(ml) => 
    //		      createRule("msqrtB"::ml.map(addRule):::"msqrtE"::Nil)
    case _ ⇒
      println("ERROR: TODO Marker: " + marker.toString)
      "'TODO'"
  }

  //converts from Set(Rule(String,List[String])) (Rules stores as tokenized strings) to List[String] (List of real grammar rules)

  def toBNF(rule: Rule): String = {
    val Rule(name, content) = rule
    content match {
      case "ref" :: to :: Nil ⇒ name + "::= " + to
      case "topLevel" :: tl ⇒
        val rest = tl.mkString(" ")
        if (rest == "") {
          "#Ignored top level rule (no rendering)"
        } else {
          name + "::= " + tl.mkString(" ")
        }
      case "contentRule" :: tl      ⇒ name + "::= " + tl.mkString(" ")
      case "Group" :: tl            ⇒ name + "::= " + "mrowB " + tl.mkString(" ") + " mrowE"
      case "empty" :: Nil           ⇒ name + "::= " + "#Empty rule"
      case List("moB", text, "moE") ⇒ name + "::= " + "moB '" + text + "' moE"
      case List("miB", text, "miE") ⇒ name + "::= " + "miB '" + text + "' miE"
      case List("mnB", text, "mnE") ⇒ name + "::= " + "mnB '" + text + "' mnE"
      case "renderB" :: prec :: tl  ⇒ name + "::= argRuleP" + prec
      //      case "rendervarB" :: tl       ⇒ name + "::= argRule"
      case "iterateB" :: tl ⇒
        val Some(delim) = content.find(x ⇒
          if (content.indexOf(x) > 0) {
            content(content.indexOf(x) - 1) == "separatorB"
          } else false)
        val Some(argName) = content.find(x ⇒
          if (content.indexOf(x) > 0) {
            content(content.indexOf(x) - 1) == "separatorE"
          } else false)
        name + "::= " + argName + " " + delim + " " + name + "_  \n" + name + "_::= " + argName + " | " + argName + " " + delim + " " + name + "_"
      case "iteratevarB" :: tl ⇒
        val Some(delim) = content.find(x ⇒
          if (content.indexOf(x) > 0) {
            content(content.indexOf(x) - 1) == "separatorB"
          } else false)
        val Some(argName) = content.find(x ⇒
          if (content.indexOf(x) > 0) {
            content(content.indexOf(x) - 1) == "separatorE"
          } else false)
        name + "::= " + argName + " " + delim + " " + name + "_  \n" + name + "_::= " + argName + " | " + argName + " " + delim + " " + name + "_"

      case "msubB" :: mainRule :: subRule :: "msubE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "msupB" :: mainRule :: subRule :: "msupE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "msubB" :: "msupB" :: mainRule :: supRule :: "msupE" ::
        subRule :: "msubE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "msupB" :: "msubB" :: mainRule :: subRule :: "msubE" ::
        supRule :: "msupE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "msubsupB" :: mainRule :: subRule :: supRule :: "msubsupE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "moverB" :: mainRule :: underRule :: "moverE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "munderB" :: mainRule :: underRule :: "munderE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "munderB" :: "moverB" :: mainRule :: overRule :: "moverE" ::
        underRule :: "munderE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "moverB" :: "munderB" :: mainRule :: underRule :: "munderE" ::
        overRule :: "moverE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "munderoverB" :: mainRule :: underRule :: overRule :: "munderoverE" :: Nil ⇒
        name + "::= " + (content mkString " ")
      case "alternatives" :: tl ⇒
        name + "::= " + (tl mkString (" | "))
      case "mtdB" :: contentRule :: "mtdE" :: Nil       ⇒ name + "::= " + content.mkString(" ")
      case "mtrB" :: contentRule :: "mtrE" :: Nil       ⇒ name + "::= " + content.mkString(" ")
      case "mtableB" :: contentRule :: "mtableE" :: Nil ⇒ name + "::= " + content.mkString(" ")
      case "mtdSingle" :: Nil                           ⇒ name + "::= mtdSingle"
      case "mtrSingle" :: Nil                           ⇒ name + "::= mtrSingle"
      case "mtableSingle" :: Nil                        ⇒ name + "::= mtableSingle"
      case "mrowSingle" :: Nil                          ⇒ name + "::= mrowSingle"
      case "msqrtB" :: tl                               ⇒ name + "::= " + content.mkString(" ")
      case List("mfracB", above, below, "mfracE")       ⇒ name + "::= " + content.mkString(" ")
      case _ ⇒
        println("TODO BNF:" + (content mkString " "))
        "'TODO'"
    }
  }

  //creates the grammar as a List[String] by adding a manually written prefix that gives the Grammar 
  //an overall shape and also includes presentation MathML parsing
  //the actual grammar is assembled in Perl by inserting '\n' in between the elements of the List.
  def getMarpaGrammar: List[String] = {
    val grammarStart =
      ":default ::= action => [name, start, length, values]" ::
        "lexeme default = latm => 1" ::
        ":start ::= Expression " ::
        "ExpressionList ::= Expression+" ::
        "Expression ::= Notation   " ::
        "             | Presentation " ::
        "Notation ::= prec0 " ::
        Nil

    val presentation =
      "Presentation ::= mrowB ExpressionList mrowE " ::
        " | moB '(' moE ExpressionList moB ')' moE " ::
        " | moB text moE " ::
        " | miB text miE " ::
        " | mnB text mnE " ::
        " | emB text emE " ::
        " | mtextB ExpressionList mtextE" ::
        " | mtextB text mtextE " ::
        " | msB text msE " ::
        " | mfracB ExpressionList mfracE" ::
        " | msqrtB Expression msqrtE" ::
        " | msupB ExpressionList msupE" ::
        " | msubB ExpressionList msubE" ::
        " | msubsupB ExpressionList msubsupE" ::
        " | munderB ExpressionList munderE " ::
        " | moverB ExpressionList moverE " ::
        " | munderoverB ExpressionList munderoverE " ::
        " | mtdB ExpressionList mtdE" ::
        " | mtrB ExpressionList mtrE" ::
        " | mtableB ExpressionList mtableE " ::
        " | mathB ExpressionList mathE " ::
        " | emB ExpressionList emE " ::
        " | mstyleB ExpressionList mstyleE " ::
        " | mrootB ExpressionList mrootE" ::
        " | mspaceB mspaceE " ::
        " | miSingle " ::
        " | moSingle " ::
        " | mtdSingle " ::
        " | mtrSingle " ::
        " | mtableSingle " ::
        " | mrowSingle " ::
        Nil

    val mathMLelements =
      "mfracB ::=  '<mfrac' attribs '>' " :: "mfracE ::=  '</mfrac>' " ::
        "msqrtB ::=  '<msqrt' attribs '>' " :: "msqrtE ::=  '</msqrt>' " ::
        "mrootB ::= '<mroot' attribs '>' " :: "mrootE ::= '</mroot>' " ::
        "msupB ::=  '<msup' attribs '>' " :: "msupE ::=  '</msup>' " ::
        "msubB ::=  '<msub' attribs '>' " :: "msubE ::=  '</msub>' " ::
        "munderB ::=  '<munder' attribs '>' " :: "munderE ::=  '</munder>' " ::
        "moverB ::=  '<mover' attribs '>' " :: "moverE ::=  '</mover>' " ::
        "mnB ::=  '<mn' attribs '>' " :: "mnE ::=  '</mn>' " ::
        "miB ::=  '<mi' attribs '>' " :: "miE ::=  '</mi>' " ::
        "msB ::=  '<ms' attribs '>' " :: "msE ::=  '</ms>' " ::
        "mspaceB ::=  '<mspace' attribs '>' " :: "mspaceE ::=  '</mspace>' " ::
        "moB ::=  '<mo' attribs '>' " :: "moE ::=  '</mo>' " ::
        "mstyleB ::=  '<mstyle' attribs '>' " :: "mstyleE ::=  '</mstyle>' " ::
        "mtextB ::=  '<mtext' attribs '>' " :: "mtextE ::=  '</mtext>' " ::
        "emB ::=  '<em' attribs '>' " :: "emE ::=  '</em>' " ::
        "mtdB ::=  '<mtd' attribs '>' " :: "mtdE ::=  '</mtd>' " ::
        "mtrB ::=  '<mtr' attribs '>' " :: "mtrE ::=  '</mtr>' " ::
        "mtableB ::=  '<mtable' attribs '>' " :: "mtableE ::=  '</mtable>' " ::
        "msubsupB ::=  '<msubsup' attribs '>' " :: "msubsupE ::=  '</msubsup>' " ::
        "munderoverB ::=  '<munderover' attribs '>' " :: "munderoverE ::=  '</munderover>' " ::
        """mrowB ::=  '<mrow' attribs '>' """ :: """mrowE ::=  '</mrow>' """ ::
        "mathB ::=  '<math' attribs '>' " :: "mathE ::=  '</math>' " ::
        "miSingle ::=  '<mi' attribs '/>'  " ::
        "moSingle ::=  '<mo' attribs '/>'  " ::
        "mtdSingle ::=  '<mtd' attribs '/>'  " ::
        "mtrSingle ::=  '<mtr' attribs '/>'  " ::
        "mtableSingle ::=  '<mtable' attribs '/>'  " ::
        "mrowSingle ::=  '<mrow' attribs '/>'  " ::
        Nil

    val lexemes =
      "ws ::= spaces" ::
        "ws ::= # empty" ::
        """spaces ~ space+""" ::
        """space ~ [\s] """ ::
        "attribs ::=  ws" ::
        "attribs::= attribRule" ::
        "attribRule ::=  attrib || attrib attribRule" ::
        """ attrib  ::= ws notEqSignS '=' ws '"' notQuoteS '"' ws""" ::
        "notEqSignS ~ notEqSign+ " ::
        "notEqSign ~ [^=<>/]" ::
        "notQuoteS ~ notQuote+ " ::
        """notQuote ~ [^"<>]""" ::
        """ text ::= textRule""" ::
        """textRule::= char | char textRule """ ::
        """ text ::= #empty""" ::
        """ char ~ [^<>]""" ::
        Nil

    val argRules = List
      .range(0, precedences.size)
      .map(prec ⇒ {
        "argRuleP" + prec + " ::= prec" + prec + " | Presentation "
      })

    val grammarCore = grammarStart ::: argRules ::: presentation ::: mathMLelements ::: lexemes
    val extractedRules = Grammar.rules.toList.map(x ⇒ Grammar.toBNF(x)).map(_.toString)

    if (NotationContent.isEmpty) {
      cleanup()
      return List("#No notations found")
    }
    if (NotationContent.size == 1) {
      val result = List("prec0 ::= " + NotationContent.head)
      cleanup()
      return result
    }
    // Take into account precedences if NotationContent.size > 1
    val notationsByPrecStrList = List
      .range(0, precedences.size)
      .map(prec ⇒ { NotationContent.filter { not ⇒ getPrecedenceFromName(not) == prec && !isOmitted(not) } })
      .toList
      .zipWithIndex
      .map({
        case (notList, prec) ⇒
          val nextPrecedence = prec + 1
          if (prec < precedences.size - 1)
            "prec" + nextPrecedence.toString :: notList
          else
            notList
      })

    val Notation = notationsByPrecStrList
      .zipWithIndex
      .map({
        case (notList, prec) ⇒
          "prec" + prec + " ::= " + notList.mkString("\n| ")
      })

    val result = grammarCore ::: Notation ::: extractedRules ::: eventList
    cleanup()
    result
  }

  def getPrecedenceFromName(ruleName: String): Int = {
    val pattern = """(?<=P)([0-9]*)(?=N[0-9]*.*)""".r
    val Some(matched) = pattern.findFirstIn(ruleName)
    matched.toInt
  }

  def cleanup() {
    eventList = List[String]();
    rules = Set[Rule]();
    NotationContent = List.empty[String]
    index = 0;
  }
}