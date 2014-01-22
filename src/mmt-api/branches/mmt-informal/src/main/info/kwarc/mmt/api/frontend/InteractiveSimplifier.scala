package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._

class InteractiveSimplifier(controller : Controller, intp : tools.nsc.interpreter.ILoop) {
  var uomLog = controller.uom.simplificationLog
  def present(state : UOMState, t : Term, rule : Rule) = {
    
  }
  def current = uomLog.head
  def rule = current._3
  var topTerm : Term = null
  def path = current._1.path
  def result = current._2
  
  def set(s : String) {
    intp.interpret("if (uom\"" + s + "\" != null) \"Success\"")
    uomLog = controller.uom.simplificationLog.reverse.filter(p => p._1 != null)
    topTerm = current._1.t
    println("Loaded Problem " + render(topTerm))
  }
  
  def next = {
    println("Simplify subterm: " + render(getSubterm(topTerm, path)))
  }
  
  def help() = {
    println("Use rule " + rule)
  }
  
  def render(t : Term) : String = {
	controller.presenter.asString(t)
  }
  
  def solve(t : Term) = {
    if (t == result) {
      println("Correct")
      topTerm = replace(topTerm, path, result)
      uomLog = uomLog.tail
      uomLog match {
        case Nil => println("Done, final result is: " + render(t)) 
        case _ => 
          println("New target is: " + render(topTerm))
          next
      }
    } else {
      println("Incorrect, Try Again")
    }
  }
  
  /** */  
  private def getSubterm(t : Term, path : List[Int]) : Term = path match {
    case Nil => t
    case hd :: tl => getSubterm(getChild(t, hd), tl)
  }
  
  private def getChild(t : Term, i : Int) = t match {
    case OMA(f,args) => if (i == 0) f else args(i - 1)
    case _ => t
  }
  
  private def replace(t : Term, path : List[Int], res : Term) : Term = path match {
    case Nil => res
    case hd :: tl => t match {
      case OMA(f, args) => 
        if (hd == 0) {
          OMA(replace(f, tl, res), args)
        } else {
          val newargs = args.zipWithIndex.map {
          case (t,i) => if (i == hd - 1) replace(t, tl, res) else t 
          }
          OMA(f, newargs)
        }
    } 
  }
  
  
}