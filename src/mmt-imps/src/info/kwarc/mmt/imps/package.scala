package info.kwarc.mmt

package object imps
{
  import info.kwarc.mmt.api.parser.SourceRef

  type SourceInfo  = Option[Either[((Int,Int,Int),(Int,Int,Int)),SourceRef]]
  type CommentInfo = Option[LineComment]

  type Required = Boolean
  val R = true
  val O = false

  def ??![T](x : Any) : T = {
    println(x)
    ???
  }

  // Slightly more helpful version of assert.
  def hAssert(b : Boolean, x : Any) : Unit = {
    if (!b) {
      println("There's an assertion about to fail. Here's some potentially useful information:")
      println(x.toString)
    }
    assert(b)
  }
}