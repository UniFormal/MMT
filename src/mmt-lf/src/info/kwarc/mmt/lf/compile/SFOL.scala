package info.kwarc.mmt.lf.compile

// covering only binary function and predicate symbols to avoid sequences
object SFOL {
   val asort  = ConstantSymbol("sort", "s", Nil)
   val sorts  = Category("S", List(asort))
   
   val funapp  = ConstantSymbol("fun", "f", List(CatRef("tm"), CatRef("tm")))
   val termvar = VariableSymbol
   val terms   = Category("tm", List(funapp, termvar)) 

   val and     = Connective("AND", List(CatRef("o"), CatRef("o")))
   val forall  = Binder("FORALL", Some(CatRef("S")), CatRef("tm"), CatRef("o"))
   val predapp = ConstantSymbol("pred", "p", List(CatRef("tm"), CatRef("tm")))
   val forms   = Category("o", List(and, forall, predapp))

   val sort  = Declaration("sort", Nil)
   val fun   = Declaration("fun", List(CatRef("S"), CatRef("S"), CatRef("S")))
   val pred  = Declaration("pred", List(CatRef("S"), CatRef("S")))

   val Syn = LogicSyntax(List(sorts, terms, forms), CatRef("o"), List(sort,fun,pred))
}