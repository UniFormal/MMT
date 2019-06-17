package info.kwarc.mmt.metamath

import scala.collection.mutable.{HashMap,Stack,TreeSet}

import org.metamath.scala._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, RelationHandler}
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.opaque._
import info.kwarc.mmt.api.opaque._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf._

class LFTranslator(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger with MMTTask with Killable {
  def logPrefix = "mm-omdoc"
  protected def report = controller.report

  val path = bt.narrationDPath

  protected val traverser = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, init: State): Term = t match {
        /*
      case ApplySpine(OMS(Metamath.forall),List(f,OMV(v))) =>
        Apply(OMS(Metamath.forall),Lambda(v,OMS(Metamath._class),Traverser(this,f)))
      case ApplySpine(OMS(Metamath.exists),List(f,OMV(v))) =>
        Apply(OMS(Metamath.exists),Lambda(v,OMS(Metamath._class),Traverser(this,f)))
      case Apply(OMS(Metamath.forall),tm) =>
        Apply(OMS(Metamath.forall),Lambda(LocalName("_"),OMS(Metamath._class),Traverser(this,tm)))
      case Apply(OMS(Metamath.exists),tm) =>
        Apply(OMS(Metamath.exists),Lambda(LocalName("_"),OMS(Metamath._class),Traverser(this,tm)))

      case ApplySpine(OMS(Metamath.forall),ls) =>
        println(ls.map(_.getClass.toString).mkString(","))
        Traverser(this,t)
        */
      case _ => Traverser(this,t)
    }
  }

  protected def doTerm(t : Term) : Term = traverser(t,())

  def addDatabase(db: Database): Document = {
    val mod = Metamath.setmm
    val doc = new Document(path)
    controller add doc
    val theory = Theory.empty(mod.doc, mod.name, Some(Metamath.prelude))
    controller add theory
    controller add MRef(doc.path, theory.path)
    val tr = new LFDBTranslator()(db)
    // TODO restricted to the first 1000 constants for inspection
    val consts = db.decls.filter{case c: Comment => false case _ => true}
    consts/*.dropRight(consts.length - 1000)*/ foreach {
      case a: Assert if a.syntax || a.typecode.id == "|-" =>
        controller add symbols.Constant(theory.toTerm, LocalName(a.label), Nil,
          Some(doTerm(tr.translateAssert(a))), tr.translateProof(a).map(doTerm), None)
      case c: Comment =>
        controller add new OpaqueText(theory.asDocument.path, OpaqueText.defaultFormat, StringFragment(c.text))
      case _ =>
    }
    val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
      throw GeneralError(s"no MMT checker found")
    }
    checker(theory)(new CheckingEnvironment(controller.simplifier,new ErrorLogger(report), RelationHandler.ignore,this))
    doc
  }
}

class LFDBTranslator(implicit db: Database) {
  val boundVars = new HashMap[Assert, Array[Option[Array[Byte]]]]
  val syntaxToDefn = new HashMap[Assert, Assert]
  val alignments = new HashMap[Assert, GlobalName]

  val SET = db.syms("set")
  val DED = db.syms("|-")
  val WN = db.asserts("wn")
  val WI = db.asserts("wi")
  val WB = db.asserts("wb")
  val WAL = db.asserts("wal")
  val CV = db.asserts("cv")
  val WCEQ = db.asserts("wceq")
  val CAB = db.asserts("cab")
/*
  boundVars += (
    (WN, Array(None)),
    (WI, Array(None, None)),
    (WB, Array(None, None)),
    (WAL, Array(None, Some(Array(1, 0)))),
    (CV, Array(Some(Array(2)))),
    (WCEQ, Array(None, None)),
    (CAB, Array(None, Some(Array(1, 0)))))
    */
/*
  alignments += (
    (WN, Metamath.not),
    (WI, Metamath.impl),
    (WB, Metamath.equiv),
    (WAL, Metamath.forall),
    (db.asserts("wa"), Metamath.and),
    (db.asserts("wo"), Metamath.or),
    (db.asserts("wtru"), Metamath.mmtrue),
    (db.asserts("wex"), Metamath.exists),
    (db.asserts("pm3.2i"), Metamath.setmm ? "andI"),
    (db.asserts("simpli"), Metamath.setmm ? "andEl"),
    (db.asserts("simpri"), Metamath.setmm ? "andEr"),
    (db.asserts("orci"), Metamath.setmm ? "orIl"),
    (db.asserts("olci"), Metamath.setmm ? "orIr"),
    (db.asserts("impl"), Metamath.setmm ? "_impl"))
*/
  db.decls foreach {
    case a: Assert => processBoundVars(a)
    case _ =>
  }

  def getBoundVars(syntax: Assert): Array[Option[Array[Byte]]] = {
    boundVars.getOrElseUpdate(syntax, {
      for (h <- syntax.hyps if h.typecode == SET)
        throw MMError(s"Syntax axiom '$syntax' is not associated to a definition. Please add it"
          + " to the exception list.")
      Array.fill(syntax.hyps.length)(None)
    })
  }

  private def newSet: TreeSet[Floating] = new TreeSet[Floating]()(Ordering.by(_.seq))

  def processBoundVars(defn: Assert) {
    defn.parse match {
      case AssertNode(eq, List(AssertNode(a, l), b)) if (eq == WB || eq == WCEQ) =>
        val used = newSet
        if (l forall {
          case HypNode(v: Floating) => used.add(v)
          case _ => false
        }) {
          val parameters = used.toArray
          boundVars.put(a, Array.tabulate(parameters.length)(i => {
            val Target = parameters(i)
            if (Target.typecode == SET) {
              val arr = new Array[Byte](parameters.length)
              def processInto(out: Array[Byte], p: ParseTree) {
                p match {
                  case HypNode(v: Floating) =>
                    parameters.indexOf(v) match {
                      case -1 =>
                      case i => out(i) = (out(i) | 2).toByte
                    }
                  case AssertNode(ax, child) =>
                    var setIndex = -1
                    for ((c, i) <- child.zipWithIndex) c match {
                      case AssertNode(Target, _) =>
                        if (setIndex != -1)
                          throw MMError(s"Definition $defn uses variable $Target twice in " +
                            "the same syntax node, which is not supported by this algorithm.")
                        setIndex = i
                      case _ =>
                    }
                    if (setIndex == -1) child.foreach(processInto(out, _))
                    else {
                      val bv = getBoundVars(ax)(setIndex).get
                      for ((b, c) <- bv zip child) {
                        if ((b & 1) != 0) {
                          val inner = new Array[Byte](out.length)
                          processInto(inner, c)
                          for (j <- 0 until out.length)
                            out(j) = (out(j) | (if (inner(j) != 0) 1 else 0)).toByte
                        }
                        if ((b & 2) != 0) processInto(out, c)
                      }
                    }
                }
              }

              processInto(arr, b)
              Some(arr)
            } else None
          }))
          syntaxToDefn.put(a, defn)
        }
      case _ =>
    }
  }

  def align(a: Assert): Term =
    OMS(alignments.getOrElse(a, Metamath.setmm ? a.label))

  type DependVars = HashMap[Floating, TreeSet[Floating]]

  def getDependVars(stmt: Assert): DependVars = {
    implicit val dependVars = new DependVars
    stmt.hyps foreach {
      case h: Floating => dependVars.put(h, newSet)
      case _ =>
    }
    val scan = new ScanExpr
    stmt.hyps foreach {
      case e: Essential => scan.scan(e.parse)
      case _ =>
    }
    scan.scan(stmt.parse)
    for ((x, y) <- stmt.disjoint) {
      if (x.activeFloat.typecode != SET)
        dependVars(x.activeFloat) -= y.activeFloat
      else if (y.activeFloat.typecode != SET)
        dependVars(y.activeFloat) -= x.activeFloat
    }
    dependVars
  }

  val LF_SET = OMS(Metamath.setmm ? "set")

  def LF_type(s: Statement): Term = s.typecode.id match {
    case "wff" => OMS(Metamath.wff)
    case _ => OMS(Metamath.setmm ? s.typecode.id)
  }

  val LF_DED = OMS(Metamath.|-)

  def LF_var(v: Floating): LocalName = LocalName(v.v.id)

  def reducedLambda(name: LocalName, tp: Term, body: Term): Term = body match {
    case Apply(left, OMV(v)) if name == v => left
    case _ => Lambda(name, tp, body)
  }

  def translateAssert(a: Assert): Term = {
    if (a.typecode != DED) {
      val bv = getBoundVars(a)
      a.parse match {
        case AssertNode(ax, child) =>
          child.zipWithIndex.foldRight(LF_type(a))((k, t) => k match {
            case (node, i) => bv(i) match {
              case Some(b) =>
                val needsFree = (b(i) & 2) != 0 ||
                  child.zipWithIndex.exists { case (c, j) => b(j) == 3 && c.stmt.typecode != SET }
                if (needsFree) Arrow(LF_SET, t) else t
              case _ =>
                Arrow(bv.foldRight(LF_type(node.stmt))((k, ty) => k match {
                  case Some(b) if (b(i) & 1) != 0 => Arrow(LF_SET, ty)
                  case _ => ty
                }), t)
            }
          })
      }
    } else {
      implicit val dependVars = getDependVars(a)

      a.hyps.foldRight(translateStmt(a))((h, t) => h match {
        case e: Essential => Arrow(translateStmt(e), t)
        case v: Floating =>
          if (v.typecode == SET) t
          else Pi(LocalName(v.v.id),
            dependVars(v).foldRight(LF_type(v))((_, ty) => Arrow(LF_SET, ty)), t)
      })
    }
  }

  def translateStmt(s: Statement)(implicit dependVars: DependVars): Term = {
    val free = newSet
    val scan = new ScanExpr(Some(free))
    scan.scan(s.parse)
    free.foldRight(Apply(LF_DED, translateTerm(s.parse)): Term)((v, t) =>
      Pi(LF_var(v), LF_SET, t))
  }

  def translateTerm(p: ParseTree)(implicit dependVars: DependVars): Term = p match {
    case HypNode(v: Floating) =>
      if (v.typecode == SET) OMV(LF_var(v))
      else dependVars.getOrElse(v, throw new IllegalArgumentException)
        .foldLeft(OMV(LF_var(v)): Term)((t, v) => Apply(t, OMV(LF_var(v))))
    case AssertNode(ax, child) =>
      val bv = getBoundVars(ax)
      child.zipWithIndex.foldLeft(align(ax))((ap, k) => k match {
        case (node, i) => bv(i) match {
          case Some(b) =>
            val needsFree = (b(i) & 2) != 0 ||
              child.zipWithIndex.exists { case (c, j) => b(j) == 3 && c.stmt.typecode != SET }
            if (needsFree) node match {
              case HypNode(v: Floating) => Apply(ap, OMV(LF_var(v)))
            }
            else ap
          case _ =>
            Apply(ap, child.zip(bv).foldRight(translateTerm(node))((k, t) => k match {
              case (HypNode(v: Floating), Some(b)) if (b(i) & 1) != 0 =>
                reducedLambda(LF_var(v), LF_SET, t)
              case _ => t
            }))
        }
      })
  }

  def translateProof(a: Assert): Option[Term] = {
    if (a.typecode == DED) return None // not currently handling proof term translation
    implicit val dependVars = getDependVars(a)
    syntaxToDefn.getOrElse(a, return None).parse match {
      case AssertNode(eq, List(AssertNode(ax, child), p)) =>
        val bv = getBoundVars(a)
        val t = try {
          translateTerm(p)
        } catch {
          case e: IllegalArgumentException => return None
        }
        Some(child.zipWithIndex.foldRight(t)((k, t) => k match {
          case (HypNode(v: Floating), i) => bv(i) match {
            case Some(b) =>
              val needsFree = (b(i) & 2) != 0 ||
                child.zipWithIndex.exists { case (c, j) => b(j) == 3 && c.stmt.typecode != SET }
              if (needsFree) Lambda(LF_var(v), LF_SET, t) else t
            case _ =>
              Lambda(LF_var(v), child.zip(bv).foldRight(LF_type(v))((k, ty) => k match {
                case (HypNode(v: Floating), Some(b)) if (b(i) & 1) != 0 => Arrow(LF_SET, ty)
                case _ => ty
              }), t)
          }
          case _ => throw ImplementationError("expected HypNode()")
        }))
    }
  }

  class ScanExpr(free: Option[TreeSet[Floating]] = None)(implicit val dependVars: DependVars) {
    var stack: List[Floating] = Nil

    def scan(p: ParseTree) {
      p match {
        case HypNode(h: Floating) => free match {
          case Some(set) => for (v <- dependVars(h) if !stack.contains(v)) set += v
          case _ if h.typecode == SET => dependVars(h) += h
          case _ => dependVars(h) ++= stack
        }
        case AssertNode(ax, child) =>
          val bv = getBoundVars(ax)
          for ((c, i) <- child.zipWithIndex if c.stmt.typecode != SET) {
            val oldLen = stack.size
            for ((d, j) <- child.zipWithIndex; arr <- bv(j) if (arr(i) & 1) != 0)
              stack = c.stmt.asInstanceOf[Floating] +: stack
            scan(c)
            while (stack.size > oldLen) stack = stack.tail
          }
      }
    }
  }
}
