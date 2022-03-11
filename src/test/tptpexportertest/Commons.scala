package tptpexportertest

object Commons {

  // Files
  sealed case class TPTPInput(inputs: Seq[Either[AnnotatedFormula, Include]]) {
    def getIncludes:Seq[Include] = inputs.filter(x => x.isRight).map(_.right.get)
    def getIncludeCount: Int = getIncludes.size
    def getFormulae:Seq[AnnotatedFormula] = inputs.filter(x => x.isLeft).map(_.left.get)
    def getFormulaeCount: Int = getFormulae.size
  }

  // Formula records
  sealed abstract class AnnotatedFormula(val name: Name, val role: Role,val annotations: Annotations) {
    type FormulaType
    def f: FormulaType

    lazy val rep = "(" + name + "," + role + "," + "(" + f.toString + ")" + annoToString(annotations) + ")."

    /**
     * Collects all Function symbols of the formula.
     * Usefull for relevance filtering
     * @return All function symbols of the formula
     */
    def function_symbols : Set[String]

    def updateRole(role: Role): AnnotatedFormula
  }

  type Annotations = Option[(Source, Seq[GeneralTerm])]
  type Role = String

  // First-order atoms
  sealed abstract class AtomicFormula {
    def function_symbols : Set[String]
    def vars: Set[String]
  }
  case class Plain(data: Func) extends AtomicFormula {
    override def toString = data.toString

    override val function_symbols: Set[String] = data.function_symbols
    def vars = data.vars
  }
  case class DefinedPlain(data: DefinedFunc) extends AtomicFormula {
    override def toString = data.toString

    override val function_symbols: Set[String] = data.function_symbols
    def vars = data.vars
  }
  case class Equality(left: Term, right: Term) extends AtomicFormula {
    override def toString = left.toString + " = " + right.toString

    override val function_symbols: Set[String] = left.function_symbols union right.function_symbols
    def vars = left.vars union right.vars
  }
  case class SystemPlain(data: SystemFunc) extends AtomicFormula {
    override def toString = data.toString

    override val function_symbols: Set[String] = data.function_symbols
    def vars = data.vars
  }

  // First-order terms
  sealed abstract class Term {
    def function_symbols : Set[String]
    def vars: Set[String]
  }
  case class Func(name: String, args: Seq[Term]) extends Term {
    override def toString = funcToString(name, args)

    override val function_symbols: Set[String] =  args.flatMap(_.function_symbols).toSet + name
    def vars = args.flatMap(_.vars).toSet
  }
  case class DefinedFunc(name: String, args: Seq[Term]) extends Term {
    override def toString = funcToString(name, args)
    override val function_symbols: Set[String] =  args.flatMap(_.function_symbols).toSet + name
    def vars = args.flatMap(_.vars).toSet
  }
  case class SystemFunc(name: String, args: Seq[Term]) extends Term {
    override def toString = funcToString(name, args)
    override val function_symbols: Set[String] =  args.flatMap(_.function_symbols).toSet + name
    def vars = args.flatMap(_.vars).toSet
  }
  case class Var(name: Variable) extends Term {
    override def toString = name.toString
    override val function_symbols: Set[String] =  Set(name)
    def vars = Set(name)
  }
  case class NumberTerm(value: Number) extends Term {
    override def toString = value.toString
    override val function_symbols: Set[String] =  Set()
    def vars = Set.empty
  }
  case class Distinct(data: String) extends Term {
    override def toString = data.toString
    override val function_symbols: Set[String] =  Set()
    def vars = Set.empty
  }

  case class Tuple(entries: Seq[Term]) extends Term {
    override def toString: Role = s"{${entries.map(_.toString).mkString(",")}"

    override def function_symbols: Set[String] = entries.flatMap(_.function_symbols).toSet
    def vars = entries.flatMap(_.vars).toSet // dont care, not used in TFF
  }

  type Variable = String

  sealed abstract class Number
  case class IntegerNumber(value: Integer) extends Number {
    override def toString = value.toString
  }
  case class DoubleNumber(value: Double) extends Number
  case class RationalNumber(p: Integer, q: Integer) extends Number {
    override def toString = p.toString + "/" + q.toString
  }

  // System terms

  // General purpose things
  type Source = GeneralTerm

  // Include directives
  type Include = (String, Seq[Name])

  // Non-logical data (GeneralTerm, General data)
  sealed case class GeneralTerm(term: Seq[Either[GeneralData, Seq[GeneralTerm]]]) {
    override def toString = term.map(gt2String).mkString(":")

    def gt2String(in: Either[GeneralData, Seq[GeneralTerm]]): String = in match {
      case Left(data) => data.toString
      case Right(termList) => "[" + termList.mkString(",") +"]"
    }

  }

  sealed abstract class GeneralData
  case class GWord(gWord: String) extends GeneralData {
    override def toString = gWord
  }
  case class GFunc(name: String, args: Seq[GeneralTerm]) extends GeneralData {
    override def toString = funcToString(name, args)
  }
  case class GVar(gVar: Variable) extends GeneralData {
    override def toString = gVar.toString
  }
  case class GNumber(gNumber: Number) extends GeneralData {
    override def toString = gNumber.toString
  }
  case class GDistinct(data: String) extends GeneralData {
    override def toString = data
  }


  // General purpose
  type Name = String


  ///////// String representation functions ///////////
  final def funcToString(name:String, args: Seq[Any]): String = if (args.isEmpty) name
  else s"$name(${args.mkString(",")})"

  final def annoToString(anno: Option[(Source, Seq[GeneralTerm])]): String = if (anno.isEmpty) ""
  else {
    val (src, termList) = anno.get
    s",${src.toString},[${termList.mkString(",")}]"
  }

  final def typedVarToString(input: (Variable,Option[Any])): String = input match {
    case (v, None) => v.toString
    case (v, Some(typ)) => s"${v.toString} : ${typ.toString}"
  }
}