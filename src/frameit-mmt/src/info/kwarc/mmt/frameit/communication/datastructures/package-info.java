/**
 * All datastructures sent from and received by the FrameIT MMT Server.
 *
 * By convention, almost all these datastructures are prefixed with an "S" standing for simplified of their
 * respective counterpart. E.g., {@link info.kwarc.mmt.frameit.communication.datastructures.SOMDoc.SOMA SOMA}
 * is the simplified variant of {@link info.kwarc.mmt.api.objects.OMA}. And for example,
 * {@link info.kwarc.mmt.frameit.communication.datastructures.DataStructures.SScroll} is the simplified variant
 * of {@link info.kwarc.mmt.frameit.business.datastructures.Scroll}.
 *
 * On the datastructure side, we use the [[https://circe.github.io/circe/ io.circe]] library to conveniently create
 * (or auto-derive with Scala magic) JSON codecs for these datastructures.
 * And on the server side, we use the [[https://github.com/finagle/finch Finch]] library to conveniently specify
 * a server with REST endpoints that accept _and_ return datastructures (i.e. Scala case classes).
 *
 * For both sides to work together, we need to make sure that in [[info.kwarc.mmt.frameit.communication.server.ServerEndpoints]] the io.circe codecs are available/imported as implicits.
 * For the io.circe codecs to be auto-derived by Scala magic, make sure that the respective case classes
 * are _sealed_ (''sealed case class Test(...)'') and that codecs for all parameter types (''...'') are also
 * auto-derivable (recursively so).
 *
 * Sometimes, we need a bit of manual Scala code to tweak or give some codec algorithms explicitly.
 * We exclusively do so in [[info.kwarc.mmt.datastructures.Codecs]].
 *
 * @author ComFreek (so far at least)
 * @todo I somewhere wrote up a debugging guide for io.circe codecs, I thought I did it as a GitHub gist under the username 'ComFreek'. I cannot find it anymore.
 */
package info.kwarc.mmt.frameit.communication.datastructures;