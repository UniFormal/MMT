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
		
		
		val reg = new MizRegistration(cluster)
		ParsingController.addToArticle(reg)
	}

	def parseRCluster(n : Node) : MizRCluster = {
	    val aid = (n \ "@aid").text
	    val nr = (n \ "@nr").text.toInt
	    
		val typ = TypeParser.parseTyp(n.child(1))
		val cluster = TypeParser.parseCluster(n.child(2))

		new MizRCluster(aid, nr, typ, cluster)
		
	}

	def parseFCluster(n : Node) : MizFCluster = {
		val aid = (n \ "@aid").text
	    val nr = (n \ "@nr").text.toInt
	  
		val args = n.child(0).child.map(x => TypeParser.parseTyp(x)).toList
		val functor = TypeParser.parseTerm(n.child(1))
		val cluster = TypeParser.parseCluster(n.child(2))


		new MizFCluster(aid, nr, functor, args, cluster)
	}

	def parseCCluster(n : Node)  : MizCCluster = {
		val aid = (n \ "@aid").text
	    val nr = (n \ "@nr").text.toInt
	  
		val typ = TypeParser.parseTyp(n.child(2))
		val first = TypeParser.parseCluster(n.child(1))
		val second = TypeParser.parseCluster(n.child(3))
		
		new MizCCluster(aid, nr, typ, first, second)
	}
	
	def parseSchemePremises(n : Node) : List[MizProposition] = {
	  n.child.map(PropositionParser.parseProposition(_)).toList
	}
	
	def parseSchemeArgs(n : Node) : MizSchemeArg = {
	  val args = n.child(0).child.map(TypeParser.parseTyp(_)).toList
	  n.label match {
	    case "SchemeFuncDecl" =>
	      val rt = TypeParser.parseTyp(n.child(1))
	      new MizSchemeFuncDecl(args, rt)
	    case "SchemePredDecl" =>
	      new MizSchemePredDecl(args)
	  }
	}
	
	
	def parseSchemeBlock(n : Node) : Unit = {
	    val snr = (n \ "@schemenr").text.toInt
		val nr = n.child.length
		val args = n.child.slice(0,nr-4).map(x => parseSchemeArgs(x)).toList
		val premises = parseSchemePremises(n.child(nr - 4))
		val prop = PropositionParser.parseProposition(n.child(nr-3))
		val sch = new MizSchemeDef(snr, args, premises, prop)
	    ParsingController.addToArticle(sch)
	}
	

}