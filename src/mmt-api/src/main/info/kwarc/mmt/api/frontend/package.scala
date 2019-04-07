package info.kwarc.mmt.api

import frontend._

/**
  * This package defines several central classes:
  * - [[Controller]] is the main MMT class.
  * - [[Shell]] is the main executable (which will create one controller).
  * - [[MMTConfig]] is the MMT configuration data structure. A controller maintains one configuration.  
  * - [[Extension]] is the addon/plugin interface of MMT. Every extension has access to one controller instance.
  * - [[Report]] handles logging, and every instance of [[Logger]] has access to a report instance.
  */
package object frontend {
}
