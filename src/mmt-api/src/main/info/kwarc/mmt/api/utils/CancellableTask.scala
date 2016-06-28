package info.kwarc.mmt.api.utils

import scala.concurrent._
import java.lang._

/**
 * Computes a value in a separate thread, thus allowing cancellation of the computation
 * 
 * @param c the code to execute asynchronously
 */
class CancellableTask[A](c: => A) {
   /** the mutable computation result, which will later be either a value or an exception */ 
   private val p = Promise[A]()
   /** the future result of the computation (immutable) */
   val future = p.future
   private val t = new Thread(new Runnable{
      def run {
         try {
            val a = c
            p.success(a)
         } catch {case e: Exception =>
            p.failure(e)
         }
      }
   })
   t.start
   /** cancels the computation
    *  
    *  This may produce inconsistent states due to half-executed side effects, but it seems to be the only way to
    *  cancel an arbitrary asynchronous computation. (The only alternative would be for the latter to poll a flag.) 
    */
   def cancel {
      t.stop
      p.failure(new Exception)
   }
}