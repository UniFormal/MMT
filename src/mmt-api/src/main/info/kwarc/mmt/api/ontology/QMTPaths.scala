package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._

object QMTPaths {
  val base = DPath(utils.mmt.baseURI / "qmt")
  val QMT: MPath = base ? "QMT"
  val QMTTypes: MPath = base ? "QMTTypes"
  val QMTQuery: MPath = base ? "QMTQuery"
  val QMTProp: MPath = base ? "QMTProp"
  val QMTRelationExp: MPath = base ? "QMTRelationExp"
  val QMTJudgements: MPath = base ? "QMTJudgements"
  val QMTBinaries: MPath = base ? "QMTBinaries"
  val QMTUnaries: MPath = base ? "QMTUnaries"
}

object QMTTypes {
  val query: GlobalName = QMTPaths.QMTTypes ? "query"
  val prop: GlobalName = QMTPaths.QMTTypes ? "prop"
  val basetp: GlobalName = QMTPaths.QMTTypes ? "basetp"
  val tp: GlobalName = QMTPaths.QMTTypes ? "tp"
  val tuple: GlobalName = QMTPaths.QMTTypes ? "tuple"
  val set: GlobalName = QMTPaths.QMTTypes ? "set"
}

object QMTQuery {
  val I: GlobalName = QMTPaths.QMTQuery ? "I"
  val Slice: GlobalName = QMTPaths.QMTQuery ? "Slice"
  val SliceFrom: GlobalName = QMTPaths.QMTQuery ? "SliceFrom"
  val SliceUntil: GlobalName = QMTPaths.QMTQuery ? "SliceUntil"
  val Element: GlobalName = QMTPaths.QMTQuery ? "Element"
  val Component: GlobalName = QMTPaths.QMTQuery ? "Component"
  val SubObject: GlobalName = QMTPaths.QMTQuery ? "SubObject"
  val Related: GlobalName = QMTPaths.QMTQuery ? "Related"
  val Literal: GlobalName = QMTPaths.QMTQuery ? "Literal"
  val Literals: GlobalName = QMTPaths.QMTQuery ? "Literals"
  val Let: GlobalName = QMTPaths.QMTQuery ? "Let"
  val Singleton: GlobalName = QMTPaths.QMTQuery ? "Singleton"
  val Paths: GlobalName = QMTPaths.QMTQuery ? "Paths"
  val Closure: GlobalName = QMTPaths.QMTQuery ? "Closure"
  val Union: GlobalName = QMTPaths.QMTQuery ? "Union"
  val Intersection: GlobalName = QMTPaths.QMTQuery ? "Intersection"
  val Difference: GlobalName = QMTPaths.QMTQuery ? "Difference"
  val Mapping: GlobalName = QMTPaths.QMTQuery ? "Mapping"
  val BigUnion: GlobalName = QMTPaths.QMTQuery ? "BigUnion"
  val Comprehension: GlobalName = QMTPaths.QMTQuery ? "Comprehension"
  val Tuple: GlobalName = QMTPaths.QMTQuery ? "Tuple"
  val Projection: GlobalName = QMTPaths.QMTQuery ? "Projection"
  val QueryFunctionApply: GlobalName = QMTPaths.QMTQuery ? "QueryFunctionApply"
}

object QMTProp {
  val IsA: GlobalName = QMTPaths.QMTProp ? "IsA"
  val PrefixOf: GlobalName = QMTPaths.QMTProp ? "PrefixOf"
  val IsIn: GlobalName = QMTPaths.QMTProp ? "IsIn"
  val IsEmpty: GlobalName = QMTPaths.QMTProp ? "IsEmpty"
  val Equal: GlobalName = QMTPaths.QMTProp ? "Equal"
  val Not: GlobalName = QMTPaths.QMTProp ? "Not"
  val And: GlobalName = QMTPaths.QMTProp ? "And"
  val Or: GlobalName = QMTPaths.QMTProp ? "Or"
  val Forall: GlobalName = QMTPaths.QMTProp ? "Forall"
  val Exists: GlobalName = QMTPaths.QMTProp ? "Exists"
  val Holds: GlobalName = QMTPaths.QMTProp ? "Holds"
}

object QMTRelationExp {
  val ToObject: GlobalName = QMTPaths.QMTRelationExp ? "ToObject"
  val ToSubject: GlobalName = QMTPaths.QMTRelationExp ? "ToSubject"
  val Transitive: GlobalName = QMTPaths.QMTRelationExp ? "Transitive"
  val Symmetric: GlobalName = QMTPaths.QMTRelationExp ? "Symmetric"
  val Choice: GlobalName = QMTPaths.QMTRelationExp ? "Choice"
  val Sequence: GlobalName = QMTPaths.QMTRelationExp ? "Sequence"
  val Reflexive: GlobalName = QMTPaths.QMTRelationExp ? "Reflexive"
  val HasType: GlobalName = QMTPaths.QMTRelationExp ? "HasType"
}

object QMTJudgements {
  val Equals: GlobalName = QMTPaths.QMTJudgements ? "Equals"
  val Types: GlobalName = QMTPaths.QMTJudgements ? "Types"
}

object QMTBinaries {
  val DependsOn: GlobalName = QMTPaths.QMTBinaries ? "DependsOn"
  val HasMeta: GlobalName = QMTPaths.QMTBinaries ? "HasMeta"
  val Includes: GlobalName = QMTPaths.QMTBinaries ? "Includes"
  val HasDomain: GlobalName = QMTPaths.QMTBinaries ? "HasDomain"
  val HasCodomain: GlobalName = QMTPaths.QMTBinaries ? "HasCodomain"
  val IsInstanceOf: GlobalName = QMTPaths.QMTBinaries ? "IsInstanceOf"
  val RefersTo: GlobalName = QMTPaths.QMTBinaries ? "RefersTo"
  val Declares: GlobalName = QMTPaths.QMTBinaries ? "Declares"
  val IsAliasFor: GlobalName = QMTPaths.QMTBinaries ? "IsAliasFor"
  val IsAlignedWith: GlobalName = QMTPaths.QMTBinaries ? "IsAlignedWith"
  val HasViewFrom: GlobalName = QMTPaths.QMTBinaries ? "HasViewFrom"
  val IsImplicitly: GlobalName = QMTPaths.QMTBinaries ? "IsImplicitly"
}

object QMTUnaries {
  val IsDocument: GlobalName = QMTPaths.QMTUnaries ? "IsDocument"
  val IsTheory: GlobalName = QMTPaths.QMTUnaries ? "IsTheory"
  val IsView: GlobalName = QMTPaths.QMTUnaries ? "IsView"
  val IsStructure: GlobalName = QMTPaths.QMTUnaries ? "IsStructure"
  val IsConstant: GlobalName = QMTPaths.QMTUnaries ? "IsConstant"
  val IsDatatypeConstructor: GlobalName = QMTPaths.QMTUnaries ? "IsDatatypeConstructor"
  val IsDataConstructor: GlobalName = QMTPaths.QMTUnaries ? "IsDataConstructor"
  val IsJudgementConstructor: GlobalName = QMTPaths.QMTUnaries ? "IsJudgementConstructor"
  val IsRule: GlobalName = QMTPaths.QMTUnaries ? "IsRule"
  val IsHighUniverse: GlobalName = QMTPaths.QMTUnaries ? "IsHighUniverse"
  val IsPattern: GlobalName = QMTPaths.QMTUnaries ? "IsPattern"
  val IsInstance: GlobalName = QMTPaths.QMTUnaries ? "IsInstance"
  val IsDerivedDeclaration: GlobalName = QMTPaths.QMTUnaries ? "IsDerivedDeclaration"
  val IsConAss: GlobalName = QMTPaths.QMTUnaries ? "IsConAss"
  val IsStrAss: GlobalName = QMTPaths.QMTUnaries ? "IsStrAss"
  val IsNotation: GlobalName = QMTPaths.QMTUnaries ? "IsNotation"
}
