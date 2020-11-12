package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.mizar.newxml.syntax.{Item, Text_Proper}

object articleTranslator {
  def translateArticle(text_Proper: Text_Proper): Unit = {
    val items = text_Proper._items map itemTranslator.translateItem
  }
}

object itemTranslator {
  def translateItem(item: Item) = {

  }
}
