/*******************************************************************************
 * This file is part of tiscaf.
 * 
 * tiscaf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Foobar is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package tiscaf

private object HResolver {

  private object errApp extends HApp {
    override def tracking = HTracking.NotAllowed
    override def keepAlive = false
    override def chunked = false
    override def buffered = false
    override def gzip = false
    def resolve(req : HReqData) = sys.error("not used")
    val hLet = new let.ErrLet(HStatus.NotFound)
  }

  def resolve(apps : Seq[HApp], req : HReqData) : (HApp, HLet) = {
    @scala.annotation.tailrec
    def doFind(rest : Seq[HApp]) : (HApp, HLet) = rest match {
      case Seq() => (errApp, errApp.hLet)
      case Seq(a, _*) => a.resolve(req) match {
        case Some(let) => (a, let)
        case None      => doFind(rest.tail)
      }
    }
    doFind(apps)
  }
}
