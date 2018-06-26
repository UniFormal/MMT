package info.kwarc.mmt

package object imps
{
  import info.kwarc.mmt.api.parser.SourceRef

  type SourceInfo  = Option[Either[((Int,Int,Int),(Int,Int,Int)),SourceRef]]
  type CommentInfo = Option[LineComment]

  trait Baz { val snafu : Int }
  implicit object Foo extends Baz {
    val snafu = 4
  }
}