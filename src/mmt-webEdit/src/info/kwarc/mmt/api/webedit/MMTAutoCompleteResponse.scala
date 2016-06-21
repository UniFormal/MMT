package info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api.utils.JSONString

class MMTAutoCompleteResponse(autocompleteOptions: List[String]) {

  def getResponse = autocompleteOptions map JSONString
}
