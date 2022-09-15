package info.kwarc.mmt.lf.compile

/** convenience class for writing a program inside Scala concrete syntax */
abstract class Program {
   /** stores the list of declarations in reverse declaration-order */
   private var decls : List[DECL] = Nil
   /** stores the list of labels for Hets file structure */
   private var labels : List[String] = Nil

   def addTag(key : String) = {
     labels = labels map { x => x match {
       case "?" => key
       case q => q
     }
     }
   }

   def getLabels() : List[String] = labels.reverse

   /** adds a declaration
    *  merges consecutive ADT's and FUNCTION's into ADTRec and FUNCTIONRec
    */
   protected def add(d: DECL): Unit = {
      decls = (d, decls) match {
         case (a: ADT, ADTRec(adts) :: tl) => ADTRec(adts ::: List(a)) :: tl
         case (a: ADT, (b: ADT) :: tl) => ADTRec(List(b,a)) :: tl
         case (f: FUNCTION, FUNCTIONRec(fs) :: tl) => FUNCTIONRec(fs ::: List(f)) :: tl
         case (f: FUNCTION, (g: FUNCTION) :: tl) => FUNCTIONRec(List(g,f)) :: tl
         case _ => { labels = "?" :: labels; d :: decls}
      }
   }

   /** returns the list of declarations in declaration-order
    *  consecutive ADT's and FUNCTION's are merged into ADTRec and FUNCTIONRec
    */
   def get : List[DECL] = decls.reverse

   /** a helper object for conveniently adding declarations via
    *  val declare(names: Seq[ID]) = (d: DECL)
    *  This adds d to the list of declarations and binds names to the ID's declared by d.
    *  The length of names must be equal to the list of introduced names.
    *  Most declarations introduce exactly one name;
    *  ADT and RECORD declarations introduce n+1 names: first the type name, then the list of constructors/fields.
    */
   protected object declare {
       def unapplySeq(d: DECL) : Option[Seq[ID]]= {
          add(d)
          val s : List[String] = d match {
             case a: ADT => a.name :: a.constructors.map(_.name)
             case r: RECORD => r.name :: r.fields.map(_.name)
             case f: FUNCTION => List(f.name)
             case t: TYPEDEF => List(t.name)
             case e: EXCEPTION => List(e.name)
             case _:COMMENT => Nil
             case _ => null
          }
//          ct += 1
          if (s == null) None else Some(s.map(ID(_)))
       }
   }
   /** checks this program */
   def check: Unit = {
      var ds: List[DECL] = get
      implicit var context = Context(Nil)
      while (ds != Nil) {
         val (d :: rest) = ds
         val newcon = DECL.check(d)
         context = context ++ newcon
         ds = rest
      }
   }
   
   def printIn(fl: FuncLang[String]) = fl.prog(get).mkString("\n")
}

/** An example program */
object TestProgram extends Program {
   import EXPConversions._
   val declare() = COMMENT("natural numbers as an inductive type")
   val declare(nat, zero, succ)  = "nat" adt ("zero".of(), "succ" of current)
   val declare() = COMMENT("rational numbers as a record")
   val declare(rat, enum, denom) = "rat" record ("enum" ::: nat, "denom" ::: nat)
   val declare() = COMMENT("type definition for a list of natural numbers")
   val declare(listnat) = "listnat" typedef LIST(nat)
   val declare() = COMMENT("addition of natural numbers")
   val declare(add) = "add" function nat <-- ("x" :: nat, "y" :: nat) =||= {
      case (x,y) => x Match (
         zero ==> y,
         succ("n") ==> succ(current("n", y))
      )
   }
   val declare() = COMMENT("multiplication of natural numbers")
   val declare(mult) = "mult" function nat <-- ("x" :: nat, "y" :: nat) =||= {
      case (x,y) => x Match (
         zero ==> zero,
         succ("n") ==> add(current("n", y), y)
      )
   }
   val declare() = COMMENT("addition of natural numbers")
   val declare(addr) = "addr" function rat <-- ("x" :: rat, "y" :: rat) =||= {case (x,y) =>
      rat(enum ::: add(mult(x __ enum, y __ denom), mult(y __ enum, x __ denom)) ,
         denom ::: mult(x __ denom, y __ denom)
         )
   }
}
