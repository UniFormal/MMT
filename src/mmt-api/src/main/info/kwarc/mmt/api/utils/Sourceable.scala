package info.kwarc.mmt.api.utils
import reflect.runtime.universe._

object Sourceable {
  def apply(obj : Any) : String = showCode(reify(obj).tree)
}
