package info.kwarc.mmt.mizar.newxml.xmlsyntax


case class Article(aid: String, mizfiles: String, _declarations: List[Declaration])


trait Declaration
trait Obj
trait FixedObj extends Obj
trait Property
trait Claim
trait Formula extends Claim with FixedObj with DefinienClause

trait Justification
trait RegistrationCluster
trait ProofStep

trait DefinitionProperties extends DefinienClause with DefinitionClause


case class Reservation(_ident: List[Ident], _tp: Typ) extends Declaration

case class Ident(vid: scala.Int) extends Declaration

case class Typ(kind: String, nr: scala.Int, pid: Option[scala.Int], vid: Option[scala.Int], _clusters: List[Cluster], _obj: List[Obj]) extends Declaration

case class Cluster(_adjectives: List[Adjective])

case class Adjective(nr: scala.Int, value: Option[Boolean], _args: List[Obj], _num: Option[Num])

case class Num(nr:scala.Int)

case class Var(nr: scala.Int) extends Obj

case class LocusVar(nr: scala.Int) extends Obj

case class Func(kind: String, nr: scala.Int, pid: Option[scala.Int], _obj:List[Obj]) extends FixedObj

case class Const(nr: scala.Int, vid: scala.Int) extends FixedObj

case class DefinitionBlock(line: scala.Int, col: scala.Int, _commonVariables: List[Let], _commonAssumptions: List[Assume], _definitions: List[Definition], _endpos: EndPosition) extends Declaration

case class EndPosition(line: scala.Int, col: scala.Int)

case class Let(nr: scala.Int, _types:List[Typ]) extends ProofStep

trait DefinitionClause

case class Definition(kind: String, nr: Option[scala.Int], vid: Option[scala.Int], line: Option[scala.Int], col:Option[scala.Int], _clauses:List[DefinitionClause])

case class Coherence(_proposition:List[Proposition], _is:Option[Is], _justification:Option[Justification]) extends DefinitionClause

case class Is(_func: Func, _typ: Typ) extends Formula

case class By(line: scala.Int, col: scala.Int, _ref:Option[Ref]) extends Justification

case class Ref(nr:scala.Int, articlenr: scala.Int, kind: String, line: scala.Int, col:scala.Int)

case class Constructor(kind: String, nr: scala.Int, aid: String, relnr: scala.Int, aggregbase:Option[scala.Int], structmodeaggrnr:Option[scala.Int], _props: Option[Properties], _argtypes:ArgTypes, _typ:List[Typ], _fields:Option[Fields]) extends DefinitionClause

case class Fields(_fields:List[Field])

case class Field(nr: scala.Int)

case class Properties(propertyarg2: scala.Int, propertyarg1: scala.Int, _property:List[Property])

case class Abstractness() extends Property

case class ArgTypes(_typs:List[Typ])

case class Pattern(kind: String, nr: scala.Int, aid: String, formatnr: scala.Int, constrkind:String, constrnr: scala.Int, relnr: scala.Int, _argtypes: ArgTypes, _visible: Visible) extends DefinitionClause

case class Visible(_int: List[Int])

case class Int(x:scala.Int)

case class Proposition(line: scala.Int, col: scala.Int, nr:Option[scala.Int], vid:Option[scala.Int], _formula: Formula) extends Claim

case class Verum() extends Formula

case class Pred(kind: String, nr: scala.Int, _num: Option[Num], _obj: List[Obj]) extends Formula

case class For(pid:scala.Int, vid: scala.Int, _typ:Typ, _formula: Formula) extends Formula

case class And(pid:scala.Int, _clauses:List[Formula]) extends Formula

case class Not(pid: scala.Int, _formula:Formula) extends Formula

case class RegistrationBlock(line: scala.Int, col: scala.Int, _commonVariables: List[Let], _registrations: List[Registration], _endpos: EndPosition) extends Declaration

case class Registration(_clusters:List[RegistrationCluster], _coherence: Option[Coherence], _existence:Option[Existence]) extends DefinitionClause

case class Existence(_prop:Proposition, _just:Justification) extends DefinitionProperties

case class Uniqueness(_prop:Proposition, _just:Justification) extends DefinitionProperties

case class FCluster(aid: String, nr: scala.Int, _argtypes:ArgTypes, _func:Func, _clusters:List[Cluster]) extends RegistrationCluster

case class CCluster(aid: String, nr: scala.Int, _argtypes:ArgTypes, _clusterss:List[Cluster], _typ:Typ, _clusters:List[Cluster]) extends RegistrationCluster

case class RCluster(aid: String, nr: scala.Int, _argtypes:ArgTypes, _typ:Typ, _clusters:List[Cluster]) extends RegistrationCluster

case class Proof(line: scala.Int, col: scala.Int, _proofItem: List[ProofStep], _endpos: EndPosition) extends Justification

case class BlockThesis(_formula:Formula) extends ProofStep

case class Set(nr: scala.Int, _func:Func, _typ:Typ) extends ProofStep

case class Conclusion(_prop:Proposition, _justification:Option[Justification]) extends ProofStep

case class Thesis(_formula: Claim, _exp: ThesisExpansions) extends ProofStep

case class ThesisExpansions(_pair: List[Pair])

case class Pair(x: scala.Int, y: scala.Int)

case class Assume(_formula: List[Claim]) extends ProofStep

case class Take(_obj: FixedObj) extends ProofStep

trait DefinienClause

case class Definiens(constrkind:String, constrnr:scala.Int, aid:String, defnr:scala.Int, relnr:scala.Int, _types:List[Typ], _defClauses:List[DefinienClause]) extends Declaration

case class DefTheorem(constrkind: String, constrnr:scala.Int, _prop:Proposition) extends Declaration

case class Essentials(_ints:List[Int]) extends DefinienClause

case class DefMeaning(_definien:FixedObj) extends DefinienClause

case class Correctness(_coherence:Option[Coherence], _claim:Claim, _just:Justification) extends DefinitionClause

case class Canceled(kind: String) extends Declaration
