package zgs.utl


object ShutHook {
  
  //----------------- API -------------------------------
  
  def register(order : Int, code : => Unit) : Unit = synchronized { hooks += (order, () => code) }
  
  //--------------- internals -----------------------------
  
  private val hooks = new scala.collection.mutable.HashSet[(Int, () => Unit)]

  Runtime.getRuntime.addShutdownHook(new Thread { override def run { 
    hooks.toSeq
         .sortWith((h1, h2) => h1._1 < h2._1)
         .foreach { h => try { h._2() } catch { case _ => } }
  }})
}
