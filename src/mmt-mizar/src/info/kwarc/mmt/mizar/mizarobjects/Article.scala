package info.kwarc.mmt.mizar.mizarobjects

class MizarArticle(val title : String, var elems : List[MizarDeclaration]) {
    def addElem(e : MizarDeclaration) {
      elems = e :: elems
  }

  def setElems(e : List[MizarDeclaration]) {
    elems = e
  }
}

abstract sealed class MizarNotation extends MizarDeclaration {}
case class MizarSynonymicNotation(arguments:MizarLocalVariable, name: MizarGlobalName, definingFormula: DefiningFormula) {}
case class MizarAntonymicNotation(arguments:MizarLocalVariable, name: MizarGlobalName, definingFormula: DefiningFormula) {}