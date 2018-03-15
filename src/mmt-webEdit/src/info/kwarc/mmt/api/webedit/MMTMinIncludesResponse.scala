package info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api.utils.{JSONString, JSON}

class MMTMinIncludesResponse(includes : List[String]) {
   def getResponse: List[JSON] = includes map JSONString
}
