package info.kwarc.mmt.mizar.mizar.reader

import scala.xml._
import info.kwarc.mmt.mizar.mizar.objects._

object SchemeRegReader {
	def parseRegistrationBlock(n : Node) = {
	    val start = UtilsReader.parseSourceRef(n)
	    val end = UtilsReader.parseSourceRef(n.child.last)
	    val sreg = Some(SourceRegion(start,end))
		val registrations = n.child.filter(_.label == "Registration").map(parseRegistration)
		registrations foreach {reg => 
	      reg.sreg = sreg
	      ParsingController.addToArticle(reg)
	    }
	}

	def parseRegistration(n : Node) : MizRegistration = {
		val c = n.child(0) //cluster
		val cluster : MizClusterDef = c.label match {
			case "RCluster" => parseRCluster(c)
			case "FCluster" => parseFCluster(c)
			case "CCluster" => parseCCluster(c)
		}
		new MizRegistration(cluster)
	}

	def parseRCluster(n : Node) : MizRCluster = {
	    val aid = (n \ "@aid").text
	    val nr = (n \ "@nr").text.toInt
	    val args = n.child(0).child.map(x => TypeParser.parseTyp(x)).toList

		val typ = TypeParser.parseTyp(n.child(1))
		val cluster = TypeParser.parseCluster(n.child(2))

		new MizRCluster(aid, nr, args, typ, cluster)
	}

	def parseFCluster(n : Node) : MizFCluster = {
		val aid = (n \ "@aid").text
	    val nr = (n \ "@nr").text.toInt
	  
		val args = n.child(0).child.map(x => TypeParser.parseTyp(x)).toList
		val functor = TypeParser.parseTerm(n.child(1))
		val cluster = TypeParser.parseCluster(n.child(2))


		new MizFCluster(aid, nr, args, functor, cluster)
	}

	def parseCCluster(n : Node)  : MizCCluster = {
		val aid = (n \ "@aid").text
	    val nr = (n \ "@nr").text.toInt
	    
	    val args = n.child(0).child.map(x => TypeParser.parseTyp(x)).toList

		val typ = TypeParser.parseTyp(n.child(2))
		val first = TypeParser.parseCluster(n.child(1))
		val second = TypeParser.parseCluster(n.child(3))
		
		new MizCCluster(aid, nr, args, typ, first, second)
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
	    //setting source region
	    val start = UtilsReader.parseSourceRef(n)
	    val end = UtilsReader.parseSourceRef(n.child.last)
	    val sreg = Some(SourceRegion(start, end))
	    sch.sreg = sreg
	    ParsingController.addToArticle(sch)
	}
}