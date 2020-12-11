/**
 * Aggregation of objects holding GlobalName/MPath/DPath information of specific archives and simple utility
 * objects with (un)apply methods.
 *
 * All objects except FrameIT are private for good reason: applications should not depend on the FrameIT archive's
 * formalizations using, say, ded or eq specifically from MitM/Foundation or MMT/urtheories. Instead, we force
 * the FrameIT *object* to reexport those symbol paths such that it is transparent to applications.
 */
package info.kwarc.mmt.frameit.archives;