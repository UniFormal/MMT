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
}

trait ViewScala extends RuleSet {
   
}
