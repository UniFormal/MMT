package info.kwarc.mmt.api.refactoring.moduleutils

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, LocalName}
import org.scalatest.FlatSpec

// TODO Refactor to use fixtures to reduce boilerplate
class ReferenceSubstituterTest extends FlatSpec {
  "Substitution of GlobalName by MPath and vice versa" should "fail" in {
    val mpath = DPath(URI("http://cds.omdoc.org/moduleutilstest")) ? "module"
    val globalName = mpath ? "test"
    val term = OMID(globalName)

    assertThrows[AssertionError] {
      ReferenceSubstituter.substitute(term, Map(globalName -> mpath))
    }

    assertThrows[AssertionError] {
      ReferenceSubstituter.substitute(term, Map(mpath -> globalName))
    }
  }

  "Substitution of GlobalName by GlobalName and MPath by MPath" should "not fail" in {
    val mpath = DPath(URI("http://cds.omdoc.org/moduleutilstest")) ? "module"
    val globalName = mpath ? "test"
    val term = OMID(globalName)

    ReferenceSubstituter.substitute(term, Map(mpath -> mpath))
    ReferenceSubstituter.substitute(term, Map(globalName -> globalName))
  }

  "Substitution of content paths in terms" should "work" in {
    // Construct the term ∀[x] x + 1
    // or more explictitly with a bit more qualified URIs:
    // ∀[x] x (source ? +) (other ? 1)

    val (term, actualSubstitutedTerm, expectedSubstitutedTerm) = {
      val sourceModulePath = DPath(URI("http://cds.omdoc.org/moduleutilstest")) ? "source"
      val otherModulePath = DPath(URI("http://cds.omdoc.org/moduleutilstest")) ? "other"
      val targetModulePath = DPath(URI("http://cds.omdoc.org/moduleutilstest")) ? "target"

      val sourceForall = OMID(sourceModulePath ? "myForall")
      val targetForall = OMID(targetModulePath ? "renamedForall")

      val sourcePlus = OMID(sourceModulePath ? "myPlus")
      val targetPlus = OMID(targetModulePath ? "myPlus")
      val otherOne = OMID(otherModulePath ? "otherOne")

      val term = OMBINDC(
        sourceForall,
        Context(VarDecl(LocalName("x"))),
        List(OMA(sourcePlus, List(OMV("x"), otherOne)))
      )

      val expectedSubstitutedTerm = OMBINDC(
        targetForall,
        Context(VarDecl(LocalName("x"))),
        List(OMA(targetPlus, List(OMV("x"), otherOne)))
      )

      val substitutions = Map(
        sourceForall.path -> targetForall.path,
        sourcePlus.path -> targetPlus.path
      )
      val actualSubstitutedTerm = ReferenceSubstituter.substitute(term, substitutions)

      (term, actualSubstitutedTerm, expectedSubstitutedTerm)
    }

    assert(actualSubstitutedTerm == expectedSubstitutedTerm)
  }

  // TODO Test with Literals and other subclasses of [[Term]]
}