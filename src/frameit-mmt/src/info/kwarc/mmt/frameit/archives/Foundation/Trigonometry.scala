package info.kwarc.mmt.frameit.archives.Foundation

object Trig {
  val trig = MitM.path ? "Trigonometry"
  def sym(s : String) = trig ? s
}

object Tan extends Unary(RealLiterals,Trig.sym("tan"),i => scala.math.tan(i.asInstanceOf[Double]))
object Sin extends Unary(RealLiterals,Trig.sym("sin"),i => scala.math.sin(i.asInstanceOf[Double]))
object Cos extends Unary(RealLiterals,Trig.sym("cos"),i => scala.math.cos(i.asInstanceOf[Double]))
object Atan extends Unary(RealLiterals,Trig.sym("atan"),i => scala.math.atan(i.asInstanceOf[Double]))
object Asin extends Unary(RealLiterals,Trig.sym("asin"),i => scala.math.asin(i.asInstanceOf[Double]))
object Acos extends Unary(RealLiterals,Trig.sym("acos"),i => scala.math.acos(i.asInstanceOf[Double]))