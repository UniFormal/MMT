package info.kwarc.mmt.mizar.mizar.reader

import scala.xml._
import info.kwarc.mmt.mizar.mizar.objects._

object SchemeRegReader {
	def parseRegistrationBlock(n : Node) = {
		n.child.filter(x => (x.label == "Registration")).map(parseRegistration)
	}

	def parseRegistration(n : Node) = {
		val c = n.child(0) //cluster
		val cluster : MizClusterDef = c.label match {
			case "RCluster" => parseRCluster(c)
			case "FCluster" => parseFCluster(c)
			case "CCluster" => parseCCluster(c)
		}
		
		val nr = (n \ "@nr").text.toInt
		cluster.setNr(nr)
		
		val reg = new MizRegistration(cluster)
		ParsingController.addToArticle(reg)
	}

	def parseRCluster(n : Node) : MizRCluster = {
		val typ = TypeParser.parseTyp(n.child(1))
		val cluster = TypeParser.parseCluster(n.child(2))

		new MizRCluster(typ, cluster)

	}

	def parseFCluster(n : Node) : MizFCluster = {
		val args = n.child(0).child.map(x => TypeParser.parseTyp(x)).toList
		val functor = TypeParser.parseTerm(n.child(1))
		val cluster = TypeParser.parseCluster(n.child(2))


		new MizFCluster(functor, args, cluster)
	}

	def parseCCluster(n : Node)  : MizCCluster = {		
		val typ = TypeParser.parseTyp(n.child(2))
		val first = TypeParser.parseCluster(n.child(1))
		val second = TypeParser.parseCluster(n.child(3))
		
		new MizCCluster(typ, first, second)
	}

}