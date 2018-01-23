package info.kwarc.mmt.lf.inhabitation

import info.kwarc.mmt.lf._

import info.kwarc.mmt.api._
import objects._
import checking._

import Inhabitation._

/**
 * P:!A, Q:!A |- P = Q
 */
object ProofIrrelevance extends SimpleIrrelevanceRule(Inh.path) //TermIrrelevanceRule(List(Apply.path), Inh.path)
