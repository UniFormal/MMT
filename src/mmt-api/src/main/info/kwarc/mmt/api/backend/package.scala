package info.kwarc.mmt.api

import backend._

/**
  * This package maintains the interface between MMT content in persistent physical storage and MMT content loaded into memory.
  *
  * The class [[Storage]] is the interface for individual physical storage containers.
  * Most of the time this corresponds to a folder containing an [[archives.Archive]].
  *
  * Content is usually stored in OMDoc XML format, which is parsed by the [[XMLStreamer]]. 
  * 
  * The class [[Backend]] maintains the registered storages and performs conversion between logical MMT URIs and physical locations.
  * 
  * The [[frontend.Controller]] owns an instance of [[Backend]].
  * Any referenced MMT URI is lazily and transparently loaded from the backend into memory and unloaded if MMT runs out of memory.
  */
package object backend {
}
