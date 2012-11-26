/**
 * abstract syntax for propositional logic with some basic patterns
 * 
 * created by Aivaras Jakubauskas a.jakubauskas@jacobs-university.de
 */
package info.kwarc.mmt.lf.compile

import info.kwarc.mmt.lf.compile._


/*
 * for testing purposes
 * PL logical syntax object that we'd get from MMT/theories/source/plWPatterns.mmt
 * 
 */

object PL {

  val cats = List(
      Connective("or",List(CatRef("bool"), CatRef("bool"))), 
      Connective("and",List(CatRef("bool"), CatRef("bool"))), 
      Connective("not",List(CatRef("bool")))
      )
  
  val consymb = ConstantSymbol("boolvar","bool",List())     
  
  val formCat = CatRef("bool")
  
  val decls = List(
		  Declaration("boolvar",List()), 
		  Declaration("axiom",List(CatRef("bool")))
  )
  

  val Syn = LogicSyntax(List(Category("bool", cats ++ List(consymb) )),formCat,decls)
}