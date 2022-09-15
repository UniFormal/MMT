package info.kwarc.mmt.oeis.parser

import scala.xml.Elem

/**
 * Created by enxhi on 4/3/15.
 */
trait Expression{
  def present : String
  //  override def toString = present
  def toNode(implicit theory : String) : Elem
}

object URIDefaults {
  val cd = "arithmetics"
  val base = "http://mathhub.info/MMT/LATIN/foundations/oeis.omdoc"
}

case class Var(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMV name={name}/>
  override def toString = "Var("+ "\""+name+"\"" + ")"
}
case class Num(double : Double) extends Expression{
  def present : String = if(double.isValidInt) double.toInt.toString else double.toString
  def toNode(implicit theory : String) : Elem = if(double.isValidInt) <OMI>{double.toInt}</OMI> else <OMF dec={double.toString}/>
}
case class Constant(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMS name={name}/> //TODO: FIGURE OUT THE RIGHT TAG
}

case class Abs(exp : Expression) extends Expression{
  def present : String = "|"+exp.present+"|"
  def toNode(implicit theory : String) = <OMS></OMS> // TODO:
}

case class Divisible(num : Expression, by : Expression) extends Expression{
  def present : String = num.toString + "|" + by
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="divisible" cd={URIDefaults.cd} base={URIDefaults.base}>
        {num.toNode}
        {by.toNode}
      </OMS>
    </OMA>
}

case class Power(base : Expression, exp : Expression) extends Expression{
  def present : String = base.toString +"^"+ exp.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="power" cd={URIDefaults.cd} base={URIDefaults.base}/>
      {base.toNode}
      {exp.toNode}
    </OMA>
}
case class Add(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" + ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="plus" cd={URIDefaults.cd} base={URIDefaults.base}/>
      {expr.map(_.toNode)}
    </OMA>
}

case class Sub(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" - ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="minus" cd={URIDefaults.cd} base={URIDefaults.base}/>
      {expr.map(_.toNode)}
    </OMA>
}

case class Mul(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("*")
  def toNode(implicit theory : String) : Elem = <OMA><OMS name="times" cd={URIDefaults.cd} base={URIDefaults.base}/>{expr.map(_.toNode)}</OMA>
}

case class Div(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("/")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="divide" cd={URIDefaults.cd} base={URIDefaults.base}/>
      {expr.map(_.toNode)}
    </OMA>
}

case class Neg(expr : Expression) extends Expression{
  def present : String = ("-"+expr.toString)
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="unary_minus" cd={URIDefaults.cd} base={URIDefaults.base}/>
      {expr.toNode}
    </OMA>
}
case class Func(name : String, args : ArgList) extends Expression{
  def present : String = name + args.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name={name} cd={URIDefaults.cd} base={URIDefaults.base}/>
      {args.args.map(_.toNode)}
    </OMA>
}
case class FuncR(seq : SeqReference, args : ArgList) extends Expression{
  def present : String = seq.toString + args.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      {seq.toNode}
      {args.args.map(_.toNode)}
    </OMA>
}

case class ExtraSymbol(symbol : String) extends Expression{
  def present : String = symbol
  def toNode(implicit theory : String) : Elem = <OMS name={symbol} cd={URIDefaults.cd} base={URIDefaults.base}></OMS>
}
case class ArgList(args : List[Expression]) extends Expression{
  def present : String = "("+args.mkString(",")+")"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS cd={URIDefaults.cd} name="set" base={URIDefaults.base}>
        {args.map(_.toNode)}
      </OMS>
    </OMA>
}
case class SeqReference(seq : String) extends Expression{
  def present : String = seq
  def toNode(implicit theory : String) : Elem = <OMR xref={seq}/>
}

case class Iters(name : String, from : Option[Expression], to : Option[Expression], on : Expression) extends Expression{
  def present : String =
    name + "_{"+(if(from.isEmpty) "" else from.get.toString)+"}^{"+(if(to.isEmpty) "" else to.get.toString)+"}("+on.toString+")"
  def toNode(implicit theory : String) : Elem =
      <OMBIND>
          <OMA>
            <OMS name={name} cd={URIDefaults.cd} base={URIDefaults.base}/>
              <OMA>
                <OMS name="interval" cd={URIDefaults.cd} base={URIDefaults.base}/>
                  {if(from.nonEmpty) {
                      from.get.toNode
                    }
                  }
                  {if(to.nonEmpty) {
                      to.get.toNode
                    }
                  }
              </OMA>
          </OMA>
          {if(from.nonEmpty) {
              from.get match {
                case Equation(eq, Var(a), rest) =>
                  <OMBVAR>{ Var(a).toNode }</OMBVAR>
                case _ => ""
              }
            }
          }
        <OMA>
          {on.toNode}
        </OMA>
      </OMBIND>
}
case class Factorial(expr : Expression) extends Expression{
  def present : String = expr.toString + "!"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="factorial" cd={URIDefaults.cd} base={URIDefaults.base}/>
      {expr.toNode}
    </OMA>
}

case class Equation(comparison : String, left : Expression, right : Expression) extends Expression{
  def present : String = left.toString + " = " + right.toString
  def toNode(implicit theory : String) =
    <OMA>
      <OMS name={comparison} cd={URIDefaults.cd} base={URIDefaults.base}/>
      {left.toNode}
      {right.toNode}
    </OMA>
}

case class Modulo(base : Expression, modulo : Expression) extends Expression {
  def present : String = base.toString + " mod " + modulo.toString
  def toNode(implicit theory : String) =
    Func("mod", ArgList(base::modulo::Nil)).toNode
}

// elemnt \in set
// example: x \in N
case class InSet(element : Expression, set : Expression) extends Expression{
  def present : String = element.toString + " in " + set.toString
  def toNode(implicit theory : String) =
    <OMA>
      <OMBVAR>{ element.toNode }</OMBVAR>
      {set.toNode}
    </OMA>
}

case class GeneratingFunction(expression: Expression) extends Expression{
  def present: String = "G.f" + expression.present

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = Func("GeneratingFunction", ArgList(List(Var("x")))).toNode
}

case class GeneratingFunctionDef(expression: ArgList) extends Expression{
  def present: String = "GF(" + expression.present + ")"

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = Func("GeneratingFunction", ArgList(List(Var("x")))).toNode
}
