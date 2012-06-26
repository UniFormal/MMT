package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._

import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.patterns._

object Patcher {
  /**
   * Applies a diff to a memory. Assumes diff is applicable!
   * @param diff the diff
   * @param mem  the memory
   */
  def patch(diff : Diff,  mem : Memory) : Unit = diff.changes.map(ch => patch(ch,mem))


  /**
   * Applies a change to a memory. Assumes the change is _applicable_!
   * @param ch the change to be applied
   * @param mem the memory representing the theory graph
   */
  private def patch(ch : Change, mem : Memory) : Unit = {
    ch match {
        
      case AddModule(m) => mem.content.add(m)
      case DeleteModule(m) => mem.content.delete(m.path)
      case AddDeclaration(d) => mem.content.add(d)
      case DeleteDeclaration(d) => mem.content.delete(d.path)
      case UpdateComponent(path, comp, old, nw) =>
        val d = mem.content.get(path)
        val dnew = updateComponent(d, comp, old, nw)
        mem.content.update(dnew)
    }

  }

  /**
   * Updates a component of a declaration
   * Assumes the component change is _applicable_ !
   * @param dec the declaration whose component is to be updated
   * @param comp the component name
   * @param old the old value of the component
   * @param nw  the new value of the component
   * @return the updated declarations
   */
  private def updateComponent(dec : ContentElement, comp : String,  old : Option[Obj], nw : Option[Obj]) : ContentElement = (dec, comp, nw) match {

    /** Theories */
    case (t : DeclaredTheory, "meta", Some(OMMOD(p))) => new DeclaredTheory(t.parent, t.name, Some(p))
    case (t : DeclaredTheory, "meta", None) => new DeclaredTheory(t.parent, t.name, None)
    case (t : DefinedTheory,  "df", Some(df : TheoryObj)) => new DefinedTheory(t.parent, t.name, df)

    /** Views */
    case (v : DeclaredView, "to", Some(to : TheoryObj)) => new DeclaredView(v.parent, v.name, v.from, to)
    case (v : DeclaredView, "from", Some(from : TheoryObj)) => new DeclaredView(v.parent, v.name, from, v.to)
    case (v : DefinedView, "to", Some(to : TheoryObj)) => new DefinedView(v.parent, v.name, v.from, to, v.df)
    case (v : DefinedView, "from", Some(from : TheoryObj)) => new DefinedView(v.parent, v.name, from, v.to, v.df)
    case (v : DefinedView,  "df", Some(df : Morph)) => new DefinedView(v.parent, v.name, v.from, v.to, df)

    /** Constants */
    case (c : Constant, "def", Some(s : Term)) => new Constant(c.home, c.name, c.tp, Some(s), c.rl)
    case (c : Constant, "def", None) => new Constant(c.home, c.name, c.tp, None, c.rl)
    case (c : Constant, "type", Some(s : Term)) => new Constant(c.home, c.name, Some(s), c.df, c.rl)
    case (c : Constant, "type", None) => new Constant(c.home, c.name, None, c.df, c.rl)

    /** Includes */
    case (i : Include,  "from", Some(from : TheoryObj)) => new Include(i.home, from)

    /** Patterns */
    case(p : Pattern,  "params", Some(params : Context)) => new Pattern(p.home, p.name, params, p.body)
    case(p : Pattern,  "con", Some(con : Context)) => new Pattern(p.home, p.name, p.params, con)

    /** Instances */
    case(i : Instance, "pattern", Some(OMID(pattern))) => new Instance(i.home,i.name, pattern, i.matches)
    case(i : Instance,  "matches", Some(matches : Substitution)) => new Instance(i.home, i.name, i.pattern, matches)

    /** ConstantAssignments */
    case(c : ConstantAssignment, "target", Some(target : Term)) => new ConstantAssignment(c.home, c.name, target)

    /** DefLinkAssignments */
    case(d : DefLinkAssignment, "target", Some(target : Morph)) => new DefLinkAssignment(d.home, d.name, target)

    /** Aliases */
    case(a : Alias, "pattern", Some(OMID(forpath))) => new Alias(a.home, a.name, forpath)

    /** Unmatched Component*/
    case _ => throw UpdateError("Unexpected component update found while applying Diff.\n" +
                                "ContentElement = " + dec.toString + "\n" +
                                "compName = " + comp +  "\n" +
                                "newComp = " + nw.toString + "\n" +
                                "(Likely caused by mismatched diff and patcher implementations)")
  }
  

}