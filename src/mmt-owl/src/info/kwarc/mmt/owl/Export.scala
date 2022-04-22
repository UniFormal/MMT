package info.kwarc.mmt.owl

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.URILiteral
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io._
import org.semanticweb.owlapi.model._
import uom.OMLiteral._

//import java.io.FileNotFoundException
//import java.io.IOException
//import java.net.UnknownHostException
//import java.util.Map import info.kwarc.mmt.api.metadata._
 import info.kwarc.mmt.api.objects._
 import info.kwarc.mmt.api.utils._
 import org.semanticweb.owlapi.vocab.OWLFacet
 import uk.ac.manchester.cs.owl.owlapi._

 import scala.collection.JavaConverters._
 import scala.collection.immutable.List

class Export(manager: OWLOntologyManager, controller: Controller) {
  private var ontology: OWLOntology = null
  private val dataFactory = new OWLDataFactoryImpl

  def globalNameToIRI(gname: GlobalName): IRI = {
    println("globalname" + gname)
    gname match {
      case (doc ? !(SimpleStep("_"))) ?? name => IRI.create(doc.toPath + "/" + name.toPath) //flat
      case _ => IRI.create(gname.toPath)
    }
  }

  def modNameToIRI(m: MPath): IRI = {
    if (m.name == LocalName("_"))
      IRI.create(m.doc.toPath)
    else
      IRI.create(m.toPath)
  }

  /**
   * translates all theories in an MMT document to ontologies
 *
   * @param doc the URI of the MMT document
   * @return the list of IRIs of the ontologies
   */

  def documentToOWL(doc: DPath): List[IRI] = {
    val document = controller.getDocument(doc)
    val modules = document.getModulesResolved(controller.library)
    modules.map { module =>
      module match {
        case module: Theory => theoryToOWL(module)
        //case module : View
      }
    }
  }

  def theoryToOWL(theory: Theory): IRI = {
    val ontoIRI = modNameToIRI(theory.path)
    ontology = manager.createOntology(ontoIRI)

    val symbols = theory.getPrimitiveDeclarations
    symbols.foreach {
      case symbol: Constant => constantToOWL(symbol)
      //case symbol : Include => IncludeToOWL(symbol)
    }
    ontoIRI
  }

  def classToOWL(t: Term): OWLClassExpression = {
    // t: Term
    t match {
      case OMS(p) => dataFactory.getOWLClass(globalNameToIRI(p))
      //case t : OMID => val p = t.gname

      case OMA(OWL2OMS("OWL2SUB", "objectIntersectionOf"), args) =>
        val argsList = args.map(classToOWL)
        dataFactory.getOWLObjectIntersectionOf(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2RL", "objectUnionOf"), args) =>
        val argsList = args.map(classToOWL)
        dataFactory.getOWLObjectUnionOf(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2QLRL", "objectComplementOf"), args) =>
        val operand = classToOWL(args(0))
        dataFactory.getOWLObjectComplementOf(operand)

      case OMA(OWL2OMS("OWL2RL", "objectAllValuesFrom"), args) =>
        val property = propertyToOWL(args(0))
        val filler = classToOWL(args(1))
        dataFactory.getOWLObjectAllValuesFrom(property, filler)

      case OMA(OWL2OMS("OWL2SUB", "objectSomeValuesFrom"), args) =>
        val property = propertyToOWL(args(0))
        val filler = classToOWL(args(1))
        dataFactory.getOWLObjectSomeValuesFrom(property, filler)

      case OMA(OWL2OMS("OWL2RL", "dataAllValuesFrom"), args) =>
        val property = dataPropertyToOWL(args(0))
        val filler = dataRangeToOWL(args(1))
        dataFactory.getOWLDataAllValuesFrom(property, filler)

      case OMA(OWL2OMS("OWL2SUB", "dataSomeValuesFrom"), args) =>
        val property = dataPropertyToOWL(args(0))
        val filler = dataRangeToOWL(args(1))
        dataFactory.getOWLDataSomeValuesFrom(property, filler)

      case OMA(OWL2OMS("OWL2RL", "objectExactCardinality"), args) =>
        val cardinality = termToInt(args(0))
        val property = propertyToOWL(args(1))
        dataFactory.getOWLObjectExactCardinality(cardinality, property)

      case OMA(OWL2OMS("OWL2RL", "objectMinCardinality"), args) =>
        val cardinality = termToInt(args(0))
        val property = propertyToOWL(args(1))
        dataFactory.getOWLObjectMinCardinality(cardinality, property)

      case OMA(OWL2OMS("OWL2RL", "objectMaxCardinality"), args) =>
        val cardinality = termToInt(args(0))
        val property = propertyToOWL(args(1))
        dataFactory.getOWLObjectMaxCardinality(cardinality, property)

      case OMA(OWL2OMS("OWL2RL", "objectExactCardinalityQualified"), args) =>
        val cardinality = termToInt(args(0))
        val property = propertyToOWL(args(1))
        val filler = classToOWL(args(2))
        dataFactory.getOWLObjectExactCardinality(cardinality, property, filler)

      case OMA(OWL2OMS("OWL2RL", "objectMinCardinalityQualified"), args) =>
        val cardinality = termToInt(args(0))
        val property = propertyToOWL(args(1))
        val filler = classToOWL(args(2))
        dataFactory.getOWLObjectMinCardinality(cardinality, property, filler)

      case OMA(OWL2OMS("OWL2RL", "objectMaxCardinalityQualified"), args) =>
        val cardinality = termToInt(args(0))
        val property = propertyToOWL(args(1))
        val filler = classToOWL(args(2))
        dataFactory.getOWLObjectMinCardinality(cardinality, property, filler)

      case OMA(OWL2OMS("OWL2RL", "dataExactCardinality"), args) =>
        val cardinality = termToInt(args(0))
        val property = dataPropertyToOWL(args(1))
        dataFactory.getOWLDataExactCardinality(cardinality, property)

      case OMA(OWL2OMS("OWL2RL", "dataMinCardinality"), args) =>
        val cardinality = termToInt(args(0))
        val property = dataPropertyToOWL(args(1))
        dataFactory.getOWLDataMinCardinality(cardinality, property)

      case OMA(OWL2OMS("OWL2RL", "dataMaxCardinality"), args) =>
        val cardinality = termToInt(args(0))
        val property = dataPropertyToOWL(args(1))
        dataFactory.getOWLDataMaxCardinality(cardinality, property)

      case OMA(OWL2OMS("OWL2RL", "dataExactCardinalityQualified"), args) =>
        val cardinality = termToInt(args(0))
        val property = dataPropertyToOWL(args(1))
        val dataRange = dataRangeToOWL(args(2))
        dataFactory.getOWLDataExactCardinality(cardinality, property, dataRange)

      case OMA(OWL2OMS("OWL2RL", "dataMinCardinalityQualified"), args) =>
        val cardinality = termToInt(args(0))
        val property = dataPropertyToOWL(args(1))
        val dataRange = dataRangeToOWL(args(2))
        dataFactory.getOWLDataMinCardinality(cardinality, property, dataRange)

      case OMA(OWL2OMS("OWL2RL", "dataMaxCardinalityQualified"), args) =>
        val cardinality = termToInt(args(0))
        val property = dataPropertyToOWL(args(1))
        val dataRange = dataRangeToOWL(args(2))
        dataFactory.getOWLDataMaxCardinality(cardinality, property, dataRange)

      case OMA(OWL2OMS("OWL2ELRL", "objectOneOf"), args) =>
        val argsList = args.map(individualToOWL)
        dataFactory.getOWLObjectOneOf(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2ELRL", "objectHasValue"), args) =>
        val property = propertyToOWL(args(0))
        val value = individualToOWL(args(1))
        dataFactory.getOWLObjectHasValue(property, value)

      case OMA(OWL2OMS("OWL2EL", "objectHasSelf"), args) =>
        val property = propertyToOWL(args(0))
        dataFactory.getOWLObjectHasSelf(property)

      case c => throw Exception("unsupported class: " + c)
    }
  }

  def individualToOWL(t: Term): OWLIndividual = {
    t match {
      case OMS(p) => dataFactory.getOWLNamedIndividual(globalNameToIRI(p))
      //case getOWLAnonymousIndividual getIDI
      case _ => throw Exception("none of the individuals")
    }
  }

  def propertyToOWL(t: Term): OWLObjectPropertyExpression = {
    t match {
      case OMS(p) => dataFactory.getOWLObjectProperty(globalNameToIRI(p))
      case OMA(OWL2OMS("OWL2QLRL", "objectInverseOf"), args) =>
        val inverse = propertyToOWL(args(0))
        dataFactory.getOWLObjectInverseOf(inverse)
      case _ => throw Exception("none of the object property expressions")
    }
  }

  def dataPropertyToOWL(t: Term): OWLDataPropertyExpression = {
    t match {
      case OMS(p) => dataFactory.getOWLDataProperty(globalNameToIRI(p))
      case _ => throw Exception("none of the data property expressions")
    }
  }

  def dataRangeToOWL(t: Term): OWLDataRange = {
    t match {
      case OMS(p) =>
        val iri: IRI = t match {
          case OWLOMS("OWL1Datatype", d1) => IRI.create("http://www.w3.org/2001/XMLSchema#" + d1)

          case OWL2OMS("OWL2Datatype", "real") => IRI.create("http://www.w3.org/2002/07/owl#real")
          case OWL2OMS("OWL2Datatype", "rational") => IRI.create("http://www.w3.org/2002/07/owl#rational")
          case OWL2OMS("OWL2Datatype", "dateTimeStamp") => IRI.create("http://www.w3.org/2001/XMLSchema#dateTimeStamp")
          case OWL2OMS("OWL2Datatype", "xmlLiteral") => IRI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral")
          case OWL2OMS("OWL2Datatype", "plainLiteral") => IRI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")
          //PlainLiteral ?
          case _ => println("other data types")
            globalNameToIRI(p)
        }
        dataFactory.getOWLDatatype(iri)

      case OMA(OWL2OMS("OWL2SUB", "dataIntersectionOf"), args) =>
        val argsList = args.map(dataRangeToOWL)
        dataFactory.getOWLDataIntersectionOf(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2", "dataUnionOf"), args) =>
        val argsList = args.map(dataRangeToOWL)
        dataFactory.getOWLDataUnionOf(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2", "dataComplementOf"), args) =>
        val dataRange = dataRangeToOWL(args(0))
        dataFactory.getOWLDataComplementOf(dataRange)

      case OMA(OWL2OMS("OWL2EL", "dataOneOf"), args) =>
        val argsList = args.map(literalToOWL)
        dataFactory.getOWLDataOneOf(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2SUB", "dataTypeRestriction"), args) =>
        val dataType = dataRangeToOWL(args(0)) match {
          case d: OWLDatatype => d
          case _ => throw Exception("not a data type")
        }
        val argsList = args.tail.map(facetToOWL)
        // argsList.toSet returns one-element set even when argsList has two different elements. Bug?
        dataFactory.getOWLDatatypeRestriction(dataType, argsList.toSet.asJava)

      case _ => throw Exception("none of the data ranges")
    }
  }

  def literalToOWL(t: Term): OWLLiteral = {
    t match {
      case OMF(lt) => dataFactory.getOWLLiteral(lt)
      case OMI(lt) => dataFactory.getOWLLiteral(lt.toInt)
      case OMSTR(lt) => dataFactory.getOWLLiteral(lt)

      case OMA(OWL2OMS("OWL2SUB", "literal"), List(OMSTR(s))) =>
        val lexicalValue = s
        dataFactory.getOWLLiteral(lexicalValue)
      case OMA(OWL2OMS("OWL2SUB", "literal"), List(OMSTR(s), OMSTR(st))) =>
        val lexicalValue = s
        val lang = st
        dataFactory.getOWLLiteral(lexicalValue, lang)

      case OMA(OWL2OMS("OWL2SUB", "literal"), List(OMSTR(s), dt)) =>
        val lexicalValue = s
        val dataType = dataRangeToOWL(dt) match {
          case dt: OWLDatatype => dt
          case _ => throw Exception("not a data type")
        }
        dataFactory.getOWLLiteral(lexicalValue, dataType)
      case _ => throw Exception("none of the literals")
    }
  }

  def facetToOWL(t: Term): OWLFacetRestriction = {
    t match {
      case OMA(OWL2OMS("OWL2SUB", "facetRestriction"), args) =>
        val facet = args(0) match {
          case OWL2OMS("OWL2SUB", f) => OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#" + f))
          //case OWL2OMS("OWL2SUB", "minInclusive") =>  OWLFacet.getFacet(IRI.create("http://www.w3.org/2001/XMLSchema#minInclusive"))
        }
        val facetValue = literalToOWL(args(1))
        dataFactory.getOWLFacetRestriction(facet, facetValue)
      // case other facets
      case _ => throw Exception("not a facet restriction")
    }
  }

  def termToInt(t: Term): Int = {
    t match {
      case OMI(i) => i.toInt
      case _ => throw Exception("not an integer")
    }
  }

  def constantToOWL(constant: Constant) {
    val mData = constant.metadata
    val mDatumList = mData.getAll
    val annotationList = mDatumList.map(annotationToOWL)

    val consName: LocalName = constant.name
    println(consName)
    val consType: Term = constant.tp match {
      case Some(t) => t
      case None => throw Exception("")
    }
    val path = constant.path

    val axiom = consType match {
      // DeclarationAxiom
      case OWLOMS("OWLBase", "class") =>
        val clss = dataFactory.getOWLClass(globalNameToIRI(path))
        dataFactory.getOWLDeclarationAxiom(clss)

      case OWLOMS("OWLBase", "individual") =>
        val individual = dataFactory.getOWLNamedIndividual(globalNameToIRI(path))
        dataFactory.getOWLDeclarationAxiom(individual)

      case OWLOMS("OWLBase", "objectProperty") =>
        val objectProperty = dataFactory.getOWLObjectProperty(globalNameToIRI(path))
        dataFactory.getOWLDeclarationAxiom(objectProperty)

      case OWLOMS("OWLBase", "dataProperty") =>
        val dataProperty = dataFactory.getOWLDataProperty(globalNameToIRI(path))
        dataFactory.getOWLDeclarationAxiom(dataProperty)

      case OWLOMS("OWL1Datatype", "dataType") =>
        val dataType = dataFactory.getOWLDatatype(globalNameToIRI(path))
        dataFactory.getOWLDeclarationAxiom(dataType)
      // ClassAxiom
      case OMA(OWL2OMS("OWL2SUB", "subClassOf"), args) =>
        val subClass = classToOWL(args(0))
        val superClass = classToOWL(args(1))
        dataFactory.getOWLSubClassOfAxiom(subClass, superClass, setAsJavaSet(annotationList.toSet))
      // java.util.Set<? extends OWLAnnotation> annotations

      case OMA(OWL2OMS("OWL2", "disjointUnionOf"), args) =>
        val firstClass = classToOWL(args(0)) match {
          case c: OWLClass => c
          case _ => throw Exception("not a class")
        }
        val argsList = args.tail.map(classToOWL)
        dataFactory.getOWLDisjointUnionAxiom(firstClass, setAsJavaSet(argsList.toSet))

      case OMA(OWL2OMS("OWL2SUB", "equivalentClasses"), args) =>
        val argsList = args.map(classToOWL)
        dataFactory.getOWLEquivalentClassesAxiom(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2SUB", "disjointClasses"), args) =>
        val argsList = args.map(classToOWL)
        dataFactory.getOWLDisjointClassesAxiom(argsList.toSet.asJava)
      // ObjectPropertyAxiom
      case OMA(OWL2OMS("OWL2SUB", "subObjectPropertyOf"), args) =>
        val subProperty = propertyToOWL(args(0))
        val superProperty = propertyToOWL(args(1))
        dataFactory.getOWLSubObjectPropertyOfAxiom(subProperty, superProperty)

      case OMA(OWL2OMS("OWL2SUB", "equivalentObjectProperties"), args) =>
        val argsList = args.map(propertyToOWL)
        dataFactory.getOWLEquivalentObjectPropertiesAxiom(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2QLRL", "disjointObjectProperties"), args) =>
        val argsList = args.map(propertyToOWL)
        dataFactory.getOWLDisjointObjectPropertiesAxiom(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2QLRL", "inverseObjectProperties"), args) =>
        val forwardProperty = propertyToOWL(args(0))
        val inverseProperty = propertyToOWL(args(1))
        dataFactory.getOWLInverseObjectPropertiesAxiom(forwardProperty, inverseProperty)

      case OMA(OWL2OMS("OWL2SUB", "objectPropertyDomain"), args) =>
        val property = propertyToOWL(args(0))
        val domain = classToOWL(args(1))
        dataFactory.getOWLObjectPropertyDomainAxiom(property, domain)

      case OMA(OWL2OMS("OWL2SUB", "objectPropertyRange"), args) =>
        val property = propertyToOWL(args(0))
        val range = classToOWL(args(1))
        dataFactory.getOWLObjectPropertyRangeAxiom(property, range)

      case OMA(OWL2OMS("OWL2RL", "functionalObjectProperty"), args) =>
        val property = propertyToOWL(args(0))
        dataFactory.getOWLFunctionalObjectPropertyAxiom(property)

      case OMA(OWL2OMS("OWL2RL", "inverseFunctionalObjectProperty"), args) =>
        val property = propertyToOWL(args(0))
        dataFactory.getOWLInverseFunctionalObjectPropertyAxiom(property)

      case OMA(OWL2OMS("OWL2ELQL", "reflexiveObjectProperty"), args) =>
        val property = propertyToOWL(args(0))
        dataFactory.getOWLReflexiveObjectPropertyAxiom(property)

      case OMA(OWL2OMS("OWL2QLRL", "irreflexiveObjectProperty"), args) =>
        val property = propertyToOWL(args(0))
        dataFactory.getOWLIrreflexiveObjectPropertyAxiom(property)

      case OMA(OWL2OMS("OWL2QLRL", "symmetricObjectProperty"), args) =>
        val property = propertyToOWL(args(0))
        dataFactory.getOWLSymmetricObjectPropertyAxiom(property)

      case OMA(OWL2OMS("OWL2QLRL", "asymmetricObjetProperty"), args) =>
        val property = propertyToOWL(args(0))
        dataFactory.getOWLAsymmetricObjectPropertyAxiom(property)

      case OMA(OWL2OMS("OWL2ELRL", "transitiveObjectProperty"), args) =>
        val property = propertyToOWL(args(0))
        dataFactory.getOWLTransitiveObjectPropertyAxiom(property)
      // DataPropertyAxiom
      case OMA(OWL2OMS("OWL2SUB", "subDataPropertyOf"), args) =>
        val subProperty = dataPropertyToOWL(args(0))
        val superProperty = dataPropertyToOWL(args(1))
        dataFactory.getOWLSubDataPropertyOfAxiom(subProperty, superProperty)

      case OMA(OWL2OMS("OWL2SUB", "equivalentDataProperties"), args) =>
        val argsList = args.map(dataPropertyToOWL)
        dataFactory.getOWLEquivalentDataPropertiesAxiom(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2QLRL", "disjointDataProperties"), args) =>
        val argsList = args.map(dataPropertyToOWL)
        dataFactory.getOWLDisjointDataPropertiesAxiom(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2SUB", "dataPropertyDomain"), args) =>
        val property = dataPropertyToOWL(args(0))
        val domain = classToOWL(args(1))
        dataFactory.getOWLDataPropertyDomainAxiom(property, domain)

      case OMA(OWL2OMS("OWL2SUB", "dataPropertyRange"), args) =>
        val property = dataPropertyToOWL(args(0))
        val range = dataRangeToOWL(args(1))
        dataFactory.getOWLDataPropertyRangeAxiom(property, range)

      case OMA(OWL2OMS("OWL2ELRL", "functionalDataProperty"), args) =>
        val property = dataPropertyToOWL(args(0))
        dataFactory.getOWLFunctionalDataPropertyAxiom(property)
      // DatatypeDefinitionAxiom
      case OMA(OWL2OMS("OWL2SUB", "dataTypeDefinition"), args) =>
        val dataType = dataRangeToOWL(args(0)) match {
          case d: OWLDatatype => d
          case _ => throw Exception("not a data type")
        }
        val dataRange = dataRangeToOWL(args(1))
        dataFactory.getOWLDatatypeDefinitionAxiom(dataType, dataRange)
      // HasKeyAxiom
      // AssertionAxiom
      case OMA(OWL2OMS("OWL2SUB", "classAssertion"), args) =>
        val clss = classToOWL(args(0))
        val individual = individualToOWL(args(1))
        dataFactory.getOWLClassAssertionAxiom(clss, individual)

      case OMA(OWL2OMS("OWL2ELRL", "sameIndividual"), args) =>
        val argsList = args.map(individualToOWL)
        dataFactory.getOWLSameIndividualAxiom(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2SUB", "differentIndividuals"), args) =>
        val argsList = args.map(individualToOWL)
        dataFactory.getOWLDifferentIndividualsAxiom(argsList.toSet.asJava)

      case OMA(OWL2OMS("OWL2SUB", "objectPropertyAssertion"), args) =>
        val property = propertyToOWL(args(0))
        val individual = individualToOWL(args(1))
        val obj = individualToOWL(args(2))
        dataFactory.getOWLObjectPropertyAssertionAxiom(property, individual, obj)

      case OMA(OWL2OMS("OWL2ELRL", "negativeObjectPropertyAssertion"), args) =>
        val property = propertyToOWL(args(0))
        val subj = individualToOWL(args(1))
        val obj = individualToOWL(args(2))
        dataFactory.getOWLNegativeObjectPropertyAssertionAxiom(property, subj, obj)

      case OMA(OWL2OMS("OWL2SUB", "dataPropertyAssertion"), args) =>
        val property = dataPropertyToOWL(args(0))
        val subj = individualToOWL(args(1))
        val obj = literalToOWL(args(2)) //   float value icin ayri method var
        dataFactory.getOWLDataPropertyAssertionAxiom(property, subj, obj)

      case OMA(OWL2OMS("OWL2ELRL", "negativeDataPropertyAssertion"), args) =>
        val property = dataPropertyToOWL(args(0))
        val subj = individualToOWL(args(1))
        val obj = literalToOWL(args(2))
        dataFactory.getOWLNegativeDataPropertyAssertionAxiom(property, subj, obj)
      // AnnotationAxiom
      //case OMA(OWL2OMS(),args) =>

      case _ => throw Exception("none of the classAxioms")
    }
    manager.addAxiom(ontology, axiom)
    //case consType : Option[OMSTR]
    //case consType : Option[OMI]
    // null
  }

  def annotationPropertyToOWL(gname: GlobalName): OWLAnnotationProperty = {
    dataFactory.getOWLAnnotationProperty(globalNameToIRI(gname))
  }

  def annotationValueToOWL(t: Term): OWLAnnotationValue = {
    t match {
      case URILiteral(i) => IRI.create(i.toString)
      case OMSTR(lt) => literalToOWL(t)
      case OMF(lt) => literalToOWL(t)
      case OMI(lt) => literalToOWL(t)
      case OMA(OWL2OMS("OWL2SUB", "literal"), _) => literalToOWL(t)
      //case OMID(ai) => individualToOWL(t)
      case _ => throw Exception("not an annotation value")
    }
  }

  def annotationToOWL(md: MetaDatum): OWLAnnotation = {
    val annotationProperty = annotationPropertyToOWL(md.key)
    val v = md.value match {
      case mv: Term => mv
      case _ => throw Exception("not a term")
    }
    val annotationValue = annotationValueToOWL(v)
    dataFactory.getOWLAnnotation(annotationProperty, annotationValue)
  }

}

object Export {

  def main(args: Array[String]) {
    implicit val eh = ErrorThrower

    val controller = new Controller
    controller.runMSLFile(utils.File("startup.mmt"), None, true, None)
    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
    val exporter = new Export(manager, controller)

    val source = utils.File(args(0))
    val target = utils.File(args(1))
    val doc: DPath = controller.read(parser.ParsingStream.fromFile(source), interpret = false).path

    def writeToFile(iri: IRI, trg: File) {
      val onto = manager.getOntology(iri)
      val file = new java.io.FileWriter(trg.toJava)
      val ontoTarget = new WriterDocumentTarget(file)
      val OWLXMLformat = new OWLXMLOntologyFormat()
      manager.saveOntology(onto, OWLXMLformat, ontoTarget)
      file.close
    }

    val iris: List[IRI] = try {
      exporter.documentToOWL(doc)
    } catch {
      case e@Exception(msg) =>
        println(msg)
        throw e
    }
    if (iris.length == 1) {
      writeToFile(iris.head, target)
    } else {
      target.toJava.mkdirs()
      iris.foreach { iri => writeToFile(iri, target / Utils.IRILast(iri)) }
    }
  }
}
