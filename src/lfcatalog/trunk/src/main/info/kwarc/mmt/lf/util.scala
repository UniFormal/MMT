package info.kwarc.mmt.lf


/** Exception thrown when a location specified does not exist on disk */
case class InexistentLocation(s : String) extends Exception(s) {
    override def toString = s
}

/** Exception related to parsing */
case class ParseError(s: String) extends Exception(s) {
    override def toString = s
}

/** Exception thrown if a file cannot be opened */
case class FileOpenError(s: String) extends Exception(s) {
    override def toString = s
}

/** Exception thrown by the catalog if a query is unsuccessul */
case class CatalogError(s: String) extends Exception(s) {
    override def toString = s
}

/** Exception thrown when the web server cannot be started because the specified port is already in use */
case class PortUnavailable(s: String) extends Exception(s) {
    override def toString = s
}


/** Current time as a string */
object Time {
    import java.util.Calendar
    /** Make sure each time component has 2 digits */
    private def formatNumber(n : Int) : String = if (n < 10) ("0" + n.toString) else n.toString
    /** Current time as a string */
    override def toString = {
        val t = Calendar.getInstance
        "[" + formatNumber(t.get(Calendar.HOUR_OF_DAY)) + ":" + formatNumber(t.get(Calendar.MINUTE)) + ":" + formatNumber(t.get(Calendar.SECOND)) + "] "
    }
}


/** For synchronization of update operations */
object ConflictGuard 


/** Checks if a port is used
  * @param port port number to be checked
  * @return true if the port is used, false if it is available */
object isTaken {
    def apply(port: Int) : Boolean = {
        var portTaken = false
        var socket : java.net.ServerSocket = null
        try {
            socket = new java.net.ServerSocket(port)
        } catch {
            case e : java.io.IOException => portTaken = true
        } finally {
            // Clean up
            if (socket != null) {
                socket.setReuseAddress(true)    // sets an OS flag that the port is available again
                socket.close
            }
        }
        return portTaken
    }
}


