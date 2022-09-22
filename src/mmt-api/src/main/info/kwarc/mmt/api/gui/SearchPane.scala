package info.kwarc.mmt.api.gui

import info.kwarc.mmt.api._
import frontend._
import parser._
import objects._
import ontology._

import javax.swing._
import tree._
import java.awt._

class SearchPane(controller: Controller) extends JPanel {
   setLayout(new BorderLayout())
   private val query = new JPanel
   private val theory = new JTextField(20)
   private val queryText = new JTextField(30)
   private val formatText = new JTextField(10)
   formatText.setText("mmt")
   private val searchButton = Swing.Button("search")(search)
   theory.setText("http://cds.omdoc.org/examples?PL")
   query.add(new JLabel("theory:"))
   query.add(theory)
   queryText.setText("$x,y: x ⇒ y")
   query.add(new JLabel("query:"))
   query.add(queryText)
   query.add(formatText)
   query.add(searchButton)
   add(query, BorderLayout.NORTH)

   private val resultRoot = new DefaultMutableTreeNode("MathWebSearchResults")
   private val resultTree = new JTree(resultRoot)
   //resultTree.setRootVisible(false)
   //private val scrollResults = new JScrollPane(results)
   add(new JScrollPane(resultTree), BorderLayout.CENTER)

   private def search: Unit = {
      val mws = controller.extman.get(classOf[MathWebSearch]).headOption.getOrElse(throw ParseError("no mws defined"))
      val q = queryText.getText
      val format = formatText.getText
      val mwsQuery = TermPattern.parse(controller, theory.getText, q, format)
      val mwsResults = mws(MathWebSearchQuery(mwsQuery))
      // node for all results in this search
      val root = new DefaultMutableTreeNode(q)
      def grouper(p: CPath) = p.parent match {
         case g: GlobalName => g.module
         case _ => p
      }
      val orderedResults = mwsResults.groupBy(r => grouper(r.cpath)).toList.sortBy(_._1.toString)
      orderedResults.foreach {
         case (mod,modAnsws) =>
            // node for all results in the module mode
            val moduleNode = new DefaultMutableTreeNode(mod.toString)
            root.add(moduleNode)
            modAnsws.groupBy(_.cpath).toList.sortBy(_._1.toString).foreach {case (cp, cpAnsws) =>
               // node for all results in the component cp
               val obj = controller.globalLookup.getComponent(cp).asInstanceOf[AbstractTermContainer].get
               val objS = obj match {
                  case Some(t) => ": " + controller.presenter.asString(t)
                  case _ => ""
               }
               val compNode = new DefaultMutableTreeNode(cp.parent.toString + "?" + cp.component.toString + objS)
               moduleNode.add(compNode)
               cpAnsws.foreach {case answ =>
                  // node for a single result
                  val sobjS = obj match {
                     case None => ""
                     case Some(t) =>
                        val sobj = t.subobject(answ.pos)._2
                        ": " + controller.presenter.asString(sobj)
                  }
                  val n = new DefaultMutableTreeNode("subterm at " + answ.pos.toString + ": " + sobjS)
                  compNode.add(n)
               }
            }
      }
      resultRoot.add(root)
      resultTree.updateUI
   }
}
