package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._


trait TheoryScala {
   val _base : DPath
   val _path : MPath
}

trait ConstantScala {
   val parent: MPath
   val name: String
   val path: GlobalName = parent ? name
   val term = objects.OMID(path)
}

trait ViewScala extends RuleSet {
   
}

object ConstantScala {
   implicit def constantToTerm(c: ConstantScala) = c.term
}