package info.kwarc.mmt.api.utils

import scala.concurrent._
import ExecutionContext.Implicits.global

import java.lang.Thread

/**
 * a flag that can be used in multi-threaded applications to signal that processing should be aborted
 * see [[Killable]]
 */

class KillButton {
  @volatile private var killed: Boolean = false
  @volatile private var action: Option[Unit => Any] = None

  def press[A](f : Unit => A) {
    killed = true
    action = Some(f)
  }
  
  def isPressed = killed
  def doAction : Any = {
    action.foreach(f => f.apply())
    action = None
  }
  
}

/** killable objects have a kill button that can be pressed to abort processing
 *  
 *  objects working with killable objects must regularly check the state of the kill button and abort gracefully if pressed
 *  
 *  multiple killable objects can share the same kill button;
 *  when creating a new killable object, it is often necessary to pass along an existing kill button
 */
trait Killable {
  private var killButton: Option[KillButton] = None
  
  /** signals aborting of processing */
  def kill[A](f : Unit => A) {
    killButton.foreach(_.press(f))
  }
  
  /** processing should be aborted gracefully if true */
  def isKilled = killButton.exists(_.isPressed)
  def killact = killButton.foreach(_.doAction)
  
  /**
   * gives a killable object the same kill button as one that is already around
   * 
   * This must be called on every newly-created killable object so that pressing the existing kill button also kills the new object 
   */
  def diesWith(implicit that: Killable): this.type = {
    this.killButton = that.killButton
    this
  }

  def newKillButton: this.type = {
    this.killButton = Some(new KillButton)
    this
  }
  
  /** presses the kill button after the specified number of milli seconds */
  def setTimeout[A](millisec: Int)(f : Unit => Unit) {
    Future {
      Thread.sleep(millisec)
      killButton.foreach(_.press(f))
    }
  }
  
}

case object TaskCancelled extends java.lang.Throwable

/**
 * Computes a value in a separate thread, thus allowing cancellation of the computation
 * 
 * @param c the code to execute asynchronously
 */
@deprecated
class CancdellableTask[A](c: => A) {
   /** the mutable computation result, which will later be either a value or an exception */ 
   private val p = Promise[A]()
   /** the future result of the computation (immutable) */
   val future = p.future
   private val t = new Thread(new Runnable{
      def run {
         try {
            val a = c
            p.success(a)
         } catch {
           case e: Exception =>
             p.failure(e)
           case e: java.lang.ThreadDeath =>
             p.failure(TaskCancelled)
         }
      }
   })
   t.start
   
   /** waits for the computation and returns the result */
   def result: scala.util.Try[A] = Await.ready(future, scala.concurrent.duration.Duration.Inf).value.get
   
   /** cancels the computation
    *  
    *  This may produce inconsistent states due to half-executed side effects, but it seems to be the only way to
    *  cancel an arbitrary asynchronous computation. (The only alternative would be for the latter to poll a flag.) 
    */
   def cancel {
      t.stop
   }
}