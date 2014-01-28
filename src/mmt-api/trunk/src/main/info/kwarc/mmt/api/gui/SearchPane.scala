package info.kwarc.mmt.api.gui

import info.kwarc.mmt.api._
import frontend._
import parser._
import objects._

import javax.swing._
import tree._
import java.awt._

class SearchPane(controller: Controller) extends JPanel {
   setLayout(new BorderLayout())
   private val query = new JPanel
   private val theory = new JTextField
   private val queryText = new JTextField
   private val searchButton = Swing.Button("search")(search)
   theory.setText("http://cds.omdoc.org/examples?PL")
   query.add(theory)
   queryText.setText("$x,y: x ⇒ y")
   query.add(queryText)
   query.add(searchButton)
   add(query, BorderLayout.NORTH)
   
   private val resultRoot = new DefaultMutableTreeNode("MathWebSearchResults")
   private val resultTree = new JTree(resultRoot)
   //resultTree.setRootVisible(false)
   //private val scrollResults = new JScrollPane(results)
   add(resultTree, BorderLayout.CENTER)
   
   private val mws = new archives.MathWebSearch(utils.URI("http://opal.eecs.jacobs-university.de:8082").toURL)
   
   private def search {
      val t = Path.parseM(theory.getText, utils.mmt.mmtcd)
      val q = queryText.getText
      val pu = ParsingUnit(SourceRef.anonymous(q), OMMOD(t), Context(), q, Some(mws.qvarNot))
      val mwsQuery = controller.termParser(pu, () => _)
      val mwsResults = mws(archives.MathWebSearchQuery(mwsQuery))
      val root = new DefaultMutableTreeNode(q)
      val orderedResults = mwsResults.map(_.cpath).groupBy(_.parent.module).toList.sortBy(_._1.toMPath.toString)
      orderedResults.foreach {
         case (k,ps) =>
            val node = new DefaultMutableTreeNode(k.toMPath.toString)
            ps.foreach {p =>
               val n = new DefaultMutableTreeNode(p.parent.toString + "?" + p.component.toString)
               node.add(n)
            }
            root.add(node)
      }
      resultRoot.add(root)
      resultTree.updateUI
   }
}
