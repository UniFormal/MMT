package info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api.utils.{JSONBoolean, JSONString, JSON}

class MMTResolveIncludesResponse(includes: List[(String, Any)]) {
  def getResponse: List[(String, JSON)] = includes.map { case (x, y) => (x, y match {
    case s: String => JSONString(s)
    case b: Boolean => JSONBoolean(b)
  })
  }
}
