package info.kwarc.mmt.jedit

import javax.swing.JToolBar
/*
MMT toolbar with useful symbols. This is a temporary
solution to the problem regarding the terminating character.
 */

public class MMTToolBar extends JPanel {

  private MMTPlugin mmtplugin

  public MMTToolBar(MMTPlugin mmtp)
  {
    mmtplugin = mmtp
    JToolBar toolBar = new JToolBar("Symbol toolbar")
    toolBar.setFloatable(false)

    toolBar.add(makeCustomButton("mmt.ins-delimiter",
      new ActionListener(){
        public void actionPerformed(ActionEvent evt) {

        }
      }
    ))

  }
}
