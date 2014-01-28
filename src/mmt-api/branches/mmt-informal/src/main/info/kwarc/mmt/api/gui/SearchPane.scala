package info.kwarc.mmt.api.gui

import info.kwarc.mmt.api._
import frontend._

import javax.swing._

class SearchPane(controller: Controller) extends JPanel {
   private val query = new JPanel
   private val queryText = new JTextField
   private val searchButton = Swing.Button("search")(search)
   query.add(queryText)
   query.add(searchButton)
   add(query)
   
   private val results = new JPanel
   add(results)

   private def search {
      val q = queryText.getText
      val res : List[Path] = List(utils.mmt.mmtcd)
      results.removeAll
      res.foreach {p =>
         val r = new JLabel(p.toString) 
         results.add(r)
      }
      revalidate
   }
}