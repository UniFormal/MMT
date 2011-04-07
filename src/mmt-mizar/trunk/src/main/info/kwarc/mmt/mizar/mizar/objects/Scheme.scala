package mizar.objects


class MizSchemeBlock(val schemes : List[MizSchemeDef]) extends MizAny

class MizSchemeDef(val args : List[MizSchemeArg], val premises : List[MizProposition], prop : MizProposition) extends MizAny


trait MizSchemeArg

class MizSchemeFunc(val argTypes : List[MizTyp], retType : MizTyp) extends MizSchemeArg

class MizSchemePred(val argTypes : List[MizTyp]) extends MizSchemeArg

