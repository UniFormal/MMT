package info.kwarc.mmt.api.objects

abstract class Continuation[State] {
	def apply(cont : Continuation[State], t : Term)(implicit con : Context, state: State) : Term
}

class OneStep[State] extends Continuation[State] {
	def apply(cont : Continuation[State], t : Term)(implicit con : Context, state : State) : Term = {
       def rec(t: Term)(implicit con : Context, state : State) = cont(this, t)(con, state)
	   t match {
		   case OMA(fun,args) => OMA(rec(fun), args.map(rec))
		   case OME(err,args) => OMA(rec(err), args.map(rec))
		   case OMM(arg,via) => OMM(rec(arg), via)
		   case OMATTR(arg,key,value) => OMATTR(rec(arg), key, rec(value))
		   case OMBINDC(b,vars,cond,body) =>
		      val newvars = vars
		      val newcon = con ++ vars
		      OMBINDC(rec(b), newvars, cond.map(x => rec(x)(newcon, state)), rec(body)(newcon, state))
		   case OMS(_) => t
		   case OMV(_) => t
		   case OMHID => t
		   case OMFOREIGN(_) => t
		   case OMI(_) => t
		   case OMSTR(_) => t
		   case OMF(_) => t
       }
   }
}

abstract class Traverser[State] extends Continuation[State] {
	def apply(t: Term, init : State, con : Context = Context()) : Term = apply(new OneStep[State], t)(con, init)
}

object PushMorphs extends Traverser[Morph] {
	def apply(cont : Continuation[Morph], t: Term)(implicit con : Context, morph : Morph) : Term = t match {
		case OMM(arg, via) => cont(this, arg)(con, morph * via)
		case OMS(path) => OMM(t, morph)
		case t => cont(this,t)
	}
	def apply(t: Term, thy : info.kwarc.mmt.api.MPath) : Term = apply(t, OMIDENT(OMMOD(thy)))
}

object Substitute extends Traverser[Substitution] {
	def apply(cont : Continuation[Substitution], t: Term)(implicit con : Context, subs: Substitution) : Term = t match {
		case OMV(n) => if (con.isDeclared(n)) OMV(n) else subs(n)
		case t => cont(this,t)
	}
}
