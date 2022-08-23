package info.kwarc.mmt.owl

import info.kwarc.mmt.api.archives.BuildResult
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.collection.JavaConverters._

//

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf._
import info.kwarc.mmt.api.uom.OMLiteral._
import info.kwarc.mmt.api.uom._

//import jomdoc.objects.{Term,OMS,OMI}

import info.kwarc.mmt.api.utils._

import scala.collection.immutable.List

//

import info.kwarc.mmt.api.metadata._

case class Exception(msg: String) extends java.lang.Throwable

class Import(manager: OWLOntologyManager, controller: Controller) {
  var num: Int = 0
  var currThy: MPath = null

  val omdocIRI: IRI = IRI.create("http://omdoc.org/identifier#id")

  def printClass(c: OWLClassExpression): Unit = {
    println(c)
  }

  def printIndividual(i: OWLIndividual): Unit = {
    println(i)
  }

  def printProperty[R <: OWLPropertyRange, P <: OWLPropertyExpression[R, P]](p: OWLPropertyExpression[R, P]): Unit = {
    println(p)
  }

  def printObject(o: OWLPropertyAssertionObject): Unit = {
    println(o)
  }

  def number(): String = {
    num = num + 1
    num.toString
  }

  def IRItoLocalName(i: IRI): LocalName = {
    LocalName(Utils.IRILast(i))
  }

  def IRItoMMTURI(i: IRI): MPath = {
    val dpath = DPath(utils.URI.fromJava(i.toURI))
    dpath ? i.toString / "_"
  }

  def ontologyToLF(ontology: OWLOntology): DPath = {
    val docIRI: IRI = manager.getOntologyDocumentIRI(ontology)
    println("from: " + docIRI)
    val ontoIRI: IRI = ontology.getOntologyID.getOntologyIRI
    println("ontology iri: " + ontoIRI)

    val docDPath = DPath(utils.URI.fromJava(docIRI.toURI))
    val ontoDPath = DPath(utils.URI.fromJava(ontoIRI.toURI))

    val document = new Document(docDPath) // phys
    controller.add(document)

    val theory = Theory.empty(ontoDPath, LocalName("_"), Some(OWL2OMS.path ? "OWL2")) //log. base, name, meta //it was OWL2Full
    controller.add(theory)
    currThy = theory.path
    controller.add(MRef(docDPath, currThy)) // phys, log

    //manager.method
    //java.util.Set<OWLOntology>    getImportsClosure(OWLOntology ontology)

    //java.util.List<OWLOntology> getSortedImportsClosure(OWLOntology ontology)
    //A list that represents a topological ordering of the imports closure. The first element in the list will be the specified ontology.

    //ontology.method
    //java.util.Set<OWLOntology>    getDirectImports()
    //java.util.Set<IRI>    getDirectImportsDocuments()
    /*
      val imports = manager.getSortedImportsClosure(ontology) // var type is java.util.List[OWLOntology]
      imports.foreach {i =>  // first element is the ontology itself ?
          val from = IRItoMMTURI(i.getOntologyID.getOntologyIRI())
          //controller.add(PlainImport(from, currThy))
          controller.add(NotationImport(from, currThy)) // ?
      }

     */
    /* val imports : Set[IRI] = ontology.getDirectImportsDocuments()
     imports.foreach { i =>
       cok onemli //if (not in controller zet) ontologyToLF(i)
              val from = IRItoMMTURI(i)
        controller.add(PlainImport(from, currThy))
     }
     */


    val axioms: List[OWLAxiom] = ontology.getAxioms.asScala.toList
    val (logicals, nonLogicals) = axioms.partition((a: OWLAxiom) => a.isLogicalAxiom)
    nonLogicals.foreach(axiom => axiomToLF(axiom))
    val individuals = ontology.getIndividualsInSignature()
    // TODO create OMDoc declarations
    // 1. for all individuals (and remove individuals from axiomToLF)
    // 2. just for those individuals that have not yet been covered by axiomToLF
    logicals.foreach(axiom => axiomToLF(axiom)) //axioms.foreach(axiom => controller.add(axiomToLF(axiom)))
    docDPath
  }

  def CurrOMS(i: IRI): Term = {
    OMID(currThy ? IRItoLocalName(i))
  }

  def addConstant(a: OWLAxiom, n: LocalName, tp: Term, md: MetaData): Unit = {
    val name =
      if (n == null) {
        a.getAnnotations.asScala.find(annot => annot.getProperty.getIRI == omdocIRI) match {
          case Some(annot) =>
            val aval = annot.getValue
            aval match {
              case aval: OWLLiteral => LocalName(aval.getLiteral)
              case _ => throw Exception("The value of the annotation has to be a literal")
            }
          case None => LocalName("ax" + tp.hashCode)
        }
      } else
        n
    val constant = Constant(OMMOD(currThy), name, Nil, Some(tp), None, None)
    //theory name: ex, class name:woman, type, none for definition
    constant.metadata = md
    controller.add(constant)

    println("constant: " + constant.toString)

  }

  def classToLF(c: OWLClassExpression): Term = {
    c match {
      case c: OWLClass => println("Class IRI: " + c.getIRI)
        CurrOMS(c.getIRI)

      case c: OWLBooleanClassExpression =>
        c match {
          case c: OWLNaryBooleanClassExpression =>
            val (sig, dec) = c match {
              case c: OWLObjectIntersectionOf => ("OWL2SUB", "objectIntersectionOf")
              case c: OWLObjectUnionOf => ("OWL2RL", "objectUnionOf")
            }
            val args = c.getOperandsAsList.asScala.map(classToLF)
            ApplySpine(OWL2OMS(sig, dec), args.toSeq: _*)

          case c: OWLObjectComplementOf =>
            val arg = c.getOperand
            ApplySpine(OWL2OMS("OWL2QLRL", "objectComplementOf"), classToLF(arg))
        }
      // OWLRestriction {
      // OWLQuantifiedRestriction {
      case c: OWLQuantifiedObjectRestriction =>
        val (sig, dec) = c match {
          case c: OWLObjectAllValuesFrom => ("OWL2RL", "objectAllValuesFrom") // hasChild son
          case c: OWLObjectSomeValuesFrom => ("OWL2SUB", "objectSomeValuesFrom") // hasPet dog
        }
        val arg1 = c.getProperty
        val arg2 = c.getFiller
        ApplySpine(OWL2OMS(sig, dec), propertyToLF(arg1), classToLF(arg2))

      case c: OWLQuantifiedDataRestriction =>
        val (sig, dec) = c match {
          case c: OWLDataAllValuesFrom => ("OWL2RL", "dataAllValuesFrom") //hasZIP integer
          case c: OWLDataSomeValuesFrom => ("OWL2SUB", "dataSomeValuesFrom")
        }
        val arg1 = c.getProperty
        val arg2 = c.getFiller
        ApplySpine(OWL2OMS(sig, dec), propertyToLF(arg1), dataRangeToLF(arg2))

      // OWLCardinalityRestriction {
      case c: OWLObjectCardinalityRestriction => //Arbitrary (Full) cardinality
        val dec = c match {
          case c: OWLObjectExactCardinality => "objectExactCardinality" //4 hasChild
          case c: OWLObjectMinCardinality => "objectMinCardinality"
          case c: OWLObjectMaxCardinality => "objectMaxCardinality"
        }
        val arg1 = c.getCardinality //non-negative integer
      val arg2 = c.getProperty

        if (c.isQualified) {
          val arg3 = c.getFiller
          ApplySpine(OWL2OMS("OWL2RL", dec + "Qualified"), OMI(arg1), propertyToLF(arg2), classToLF(arg3)) //4 hasChild parent
        }
        else
          ApplySpine(OWL2OMS("OWL2RL", dec), OMI(arg1), propertyToLF(arg2))

      case c: OWLDataCardinalityRestriction =>
        val dec = c match {
          case c: OWLDataExactCardinality => "dataExactCardinality"
          case c: OWLDataMinCardinality => "dataMinCardinality"
          case c: OWLDataMaxCardinality => "dataMaxCardinality"
        }
        val arg1 = c.getCardinality //non-negative integer
      val arg2 = c.getProperty

        if (c.isQualified) {
          val arg3 = c.getFiller
          ApplySpine(OWL2OMS("OWL2RL", dec + "Qualified"), OMI(arg1), propertyToLF(arg2), dataRangeToLF(arg3))
        }
        else
          ApplySpine(OWL2OMS("OWL2RL", dec), OMI(arg1), propertyToLF(arg2))
      //}
      //}
      case c: OWLObjectHasValue =>
        val arg1 = c.getProperty
        val arg2 = c.getValue
        ApplySpine(OWL2OMS("OWL2ELRL", "objectHasValue"), propertyToLF(arg1), individualToLF(arg2))

      case c: OWLObjectHasSelf =>
        val arg = c.getProperty
        ApplySpine(OWL2OMS("OWL2EL", "objectHasSelf"), propertyToLF(arg))
      //}
      case c: OWLObjectOneOf =>
        val args = c.getIndividuals.asScala.map(individualToLF)
        ApplySpine(OWL2OMS("OWL2ELRL", "objectOneOf"), args.toList: _*)
    }
  }

  def individualToLF(i: OWLIndividual): Term = {
    i match {
      case i: OWLNamedIndividual =>
        CurrOMS(i.getIRI)
      /* case i : OWLAnonymousIndividual => i.getIDI */
    }
  }

  def propertyToLF[R <: OWLPropertyRange, P <: OWLPropertyExpression[R, P]](p: OWLPropertyExpression[R, P]): Term = {
    p match {
      case p: OWLObjectPropertyExpression =>
        p match {
          case p: OWLObjectProperty =>
            CurrOMS(p.getIRI)

          case p: OWLObjectInverseOf =>
            val arg = p.getInverse()
            ApplySpine(OWL2OMS("OWL2QLRL", "objectInverseOf"), propertyToLF(arg))
        }

      case p: OWLDataPropertyExpression =>
        p match {
          case p: OWLDataProperty =>
            CurrOMS(p.getIRI)
        }
    }
  }

  def literalToLF(lt: OWLLiteral): Term = {
    if (lt.isDouble())
      OMF(lt.parseDouble())
    else if (lt.isFloat())
      OMF(lt.parseFloat())
    else if (lt.isInteger())
      OMI(lt.parseInteger())
    //else if (lt.isBoolean()) lt.parseBoolean()
    else if (lt.isRDFPlainLiteral) {
      if (lt.hasLang)
        ApplySpine(OWL2OMS("OWL2SUB", "literal"), OMSTR(lt.getLiteral), OMSTR(lt.getLang))
      else
        ApplySpine(OWL2OMS("OWL2SUB", "literal"), OMSTR(lt.getLiteral))
    }
    else
      ApplySpine(OWL2OMS("OWL2SUB", "literal"), OMSTR(lt.getLiteral), dataRangeToLF(lt.getDatatype))
    //throw Exception("none of the literals")
    //datatype attribution to remember which type
  }

  def facetToLF(f: OWLFacetRestriction): Term = {
    val arg1 = f.getFacet()
    val dec = IRItoLocalName(arg1.getIRI)
    val arg2 = f.getFacetValue()
    ApplySpine(OWL2OMS("OWL2SUB", "facetRestriction"), OWL2OMS("OWL2SUB", dec.toString), literalToLF(arg2))
  }

  def dataRangeToLF(dr: OWLDataRange): Term = {
    dr match {
      case dr: OWLDatatype =>
        if (dr.isBuiltIn()) {
          val dt = dr.getBuiltInDatatype()
          dt.getShortName match {
            case "real" => OWL2OMS("OWL2Datatype", dt.getShortName)
            case "rational" => OWL2OMS("OWL2Datatype", dt.getShortName)
            case "xmlLiteral" => OWL2OMS("OWL2Datatype", dt.getShortName)
            //case "plainLiteral" => OWL2OMS("OWL2Datatype",dt.getShortName)
            case "dateTimeStamp" => OWL2OMS("OWL2Datatype", dt.getShortName)
            case _ => OWLOMS("OWL1Datatype", dt.getShortName)
          }
        }
        else if (dr.isRDFPlainLiteral())
          OWL2OMS("OWL2Datatype", "PlainLiteral")

        /*     else if(dr.isBoolean())
                 OWLOMS("OWL1Datatype","boolean")
           else if(dr.isDouble())
                 OWLOMS("OWL1Datatype","double")
           else if(dr.isFloat())
                 OWLOMS("OWL1Datatype","float")
           else if(dr.isInteger())  //dataRange is "integer"
                 OWLOMS("OWL1Datatype","integer")
           else if(dr.isRDFPlainLiteral())
                 OWL2OMS("OWL2Datatype","PlainLiteral") //RDFPlainLiteral
           else if(dr.isString())
                 OWLOMS("OWL1Datatype","string")
        */
        else
          CurrOMS(dr.getIRI) // userdefined datatypes
      //throw Exception("none of the data types")

      case dr: OWLNaryDataRange =>
        val (sig, dec) = dr match {
          case dr: OWLDataIntersectionOf => ("OWL2SUB", "dataIntersectionOf")
          case dr: OWLDataUnionOf => ("OWL2", "dataUnionOf")
        }
        val args = dr.getOperands.asScala.map(dataRangeToLF)
        ApplySpine(OWL2OMS(sig, dec), args.toList: _*)

      case dr: OWLDataComplementOf =>
        val arg = dr.getDataRange()
        ApplySpine(OWL2OMS("OWL2", "dataComplementOf"), dataRangeToLF(arg))

      case dr: OWLDataOneOf => // "Peter" "1"^^xsd:integer
        val args = dr.getValues.asScala.map(literalToLF)
        ApplySpine(OWL2OMS("OWL2EL", "dataOneOf"), args.toList: _*)

      case dr: OWLDatatypeRestriction => //xsd:integer xsd:minInclusive "5"^^xsd:integer xsd:maxExclusive "10"^^xsd:integer
        val arg1 = dr.getDatatype
        val args = dr.getFacetRestrictions.asScala.map(facetToLF)
        ApplySpine(OWL2OMS("OWL2SUB", "dataTypeRestriction"), dataRangeToLF(arg1) :: args.toList: _*)
    }
  }

  def axiomToLF(ax: OWLAxiom) = {
    // def axiomToLF(ax : OWLAxiom) : Constant = {} constant donduruyor mu kontrol et
    val (name, tp) =
      ax match {
        /*   if(isAnnotated()) AnnotationToLF(ax) */
        // DeclarationAxiom
        case ax: OWLDeclarationAxiom =>
          //println(ax.getAxiomType)
          val entity: OWLEntity = ax.getEntity
          val tp: Term =
            if (entity.isBuiltIn())
            //a datatype and the IRI is rdfs:Literal or is in the OWL 2 datatype map or is rdf:PlainLiteral
              OWL2OMS("signame", "BuiltIn") // have to extend this
            else if (entity.isOWLAnnotationProperty()) // comment
              OWL2OMS("OWL2SUB", "annotationProperty") // type , module and name
            else if (entity.isOWLDataProperty) // hasAge
              OWLOMS("OWLBase", "dataProperty")
            else if (entity.isOWLDatatype)
              OWLOMS("OWL1Datatype", "dataType")
            else if (entity.isOWLClass) // Woman
              OWLOMS("OWLBase", "class")
            else if (entity.isOWLNamedIndividual) // John
              OWLOMS("OWLBase", "individual")
            else if (entity.isOWLObjectProperty) // hasWife
              OWLOMS("OWLBase", "objectProperty")
            else throw Exception("none of the entities")

          val name = IRItoLocalName(entity.getIRI)
          (name, tp)

        // ClassAxiom
        case ax: OWLClassAxiom =>
          ax match {
            case ax: OWLSubClassOfAxiom =>
              //val name = LocalName("ax" + number()) deleted
              val arg1 = ax.getSubClass()
              val arg2 = ax.getSuperClass()
              val tp = ApplySpine(OWL2OMS("OWL2SUB", "subClassOf"), classToLF(arg1), classToLF(arg2))
              (null, tp)

            case ax: OWLDisjointUnionAxiom =>
              val arg1 = ax.getOWLClass
              val args = ax.getClassExpressions.asScala.map(classToLF)
              val tp = ApplySpine(OWL2OMS("OWL2", "disjointUnionOf"), classToLF(arg1) :: args.toList: _*)
              (null, tp)

            case ax: OWLNaryClassAxiom =>
              val (sig, dec) = ax match {
                case ax: OWLEquivalentClassesAxiom => ("OWL2SUB", "equivalentClasses")
                case ax: OWLDisjointClassesAxiom => ("OWL2SUB", "disjointClasses")
              }
              val args = ax.getClassExpressionsAsList.asScala.map(classToLF)
              val tp = ApplySpine(OWL2OMS(sig, dec), args.toSeq: _*)
              (null, tp)
          }
        // ObjectPropertyAxiom
        case ax: OWLSubObjectPropertyOfAxiom =>
          val arg1 = ax.getSubProperty()
          val arg2 = ax.getSuperProperty()
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "subObjectPropertyOf"), propertyToLF(arg1), propertyToLF(arg2))
          (null, tp)

        //case ax : OWLNaryPropertyAxiom => {
        case ax: OWLEquivalentObjectPropertiesAxiom =>
          val args = ax.getProperties.asScala.map(propertyToLF)
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "equivalentObjectProperties"), args.toList: _*)
          (null, tp)

        case ax: OWLDisjointObjectPropertiesAxiom =>
          val args = ax.getProperties.asScala.map(propertyToLF)
          val tp = ApplySpine(OWL2OMS("OWL2QLRL", "disjointObjectProperties"), args.toList: _*)
          (null, tp)

        case ax: OWLInverseObjectPropertiesAxiom =>
          val arg1 = ax.getFirstProperty()
          val arg2 = ax.getSecondProperty()
          val tp = ApplySpine(OWL2OMS("OWL2QLRL", "inverseObjectProperties"), propertyToLF(arg1), propertyToLF(arg2)) //?
          (null, tp)
        //}
        case ax: OWLObjectPropertyDomainAxiom =>
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "objectPropertyDomain"), propertyToLF(ax.getProperty), classToLF(ax.getDomain))
          (null, tp)

        case ax: OWLObjectPropertyRangeAxiom => //hasWife woman
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "objectPropertyRange"), propertyToLF(ax.getProperty), classToLF(ax.getRange)) // R? <- getRange
          (null, tp)

        case ax: OWLObjectPropertyCharacteristicAxiom =>
          val (sig, dec) = ax match {
            case ax: OWLFunctionalObjectPropertyAxiom => ("OWL2RL", "functionalObjectProperty")
            case ax: OWLInverseFunctionalObjectPropertyAxiom => ("OWL2RL", "inverseFunctionalObjectProperty")
            case ax: OWLReflexiveObjectPropertyAxiom => ("OWL2ELQL", "reflexiveObjectProperty")
            case ax: OWLIrreflexiveObjectPropertyAxiom => ("OWL2QLRL", "irreflexiveObjectProperty")
            case ax: OWLSymmetricObjectPropertyAxiom => ("OWL2QLRL", "symmetricObjectProperty")
            case ax: OWLAsymmetricObjectPropertyAxiom => ("OWL2QLRL", "asymmetricObjetProperty")
            case ax: OWLTransitiveObjectPropertyAxiom => ("OWL2ELRL", "transitiveObjectProperty")
          }
          val tp = ApplySpine(OWL2OMS(sig, dec), propertyToLF(ax.getProperty))
          (null, tp)
        // DataPropertyAxiom
        case ax: OWLSubDataPropertyOfAxiom =>
          val arg1 = ax.getSubProperty()
          val arg2 = ax.getSuperProperty()
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "subDataPropertyOf"), propertyToLF(arg1), propertyToLF(arg2))
          (null, tp)

        case ax: OWLEquivalentDataPropertiesAxiom =>
          val args = ax.getProperties.asScala.map(propertyToLF)
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "equivalentDataProperties"), args.toList: _*)
          (null, tp)

        case ax: OWLDisjointDataPropertiesAxiom => //hasName  hasAddress
          val args = ax.getProperties.asScala.map(propertyToLF)
          val tp = ApplySpine(OWL2OMS("OWL2QLRL", "disjointDataProperties"), args.toList: _*)
          (null, tp)

        case ax: OWLDataPropertyDomainAxiom =>
          val arg1 = ax.getProperty()
          val arg2 = ax.getDomain
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "dataPropertyDomain"), propertyToLF(arg1), classToLF(arg2))
          (null, tp)

        case ax: OWLDataPropertyRangeAxiom =>
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "dataPropertyRange"), propertyToLF(ax.getProperty), dataRangeToLF(ax.getRange))
          (null, tp)

        case ax: OWLFunctionalDataPropertyAxiom => //hasAge
          val tp = ApplySpine(OWL2OMS("OWL2ELRL", "functionalDataProperty"), propertyToLF(ax.getProperty))
          (null, tp)
        // DatatypeDefinitionAxiom
        case ax: OWLDatatypeDefinitionAxiom =>
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "dataTypeDefinition"), dataRangeToLF(ax.getDatatype), dataRangeToLF(ax.getDataRange))
          (null, tp)

        // HasKeyAxiom     /*  case ax : OWLHasKeyAxiom =>  */
        //each named instance of a class is uniquely identified by a (data or object) property or a set of properties
        //hasKey: class -> objectProperty(or dataProperty) -> type.


        // AssertionAxiom
        case ax: OWLClassAssertionAxiom =>
          val tp = ApplySpine(OWL2OMS("OWL2SUB", "classAssertion"), classToLF(ax.getClassExpression), individualToLF(ax.getIndividual))
          (null, tp)

        case ax: OWLNaryIndividualAxiom =>
          val (sig, dec) = ax match {
            case ax: OWLSameIndividualAxiom => ("OWL2ELRL", "sameIndividual")
            case ax: OWLDifferentIndividualsAxiom => ("OWL2SUB", "differentIndividuals")
          }
          val args = ax.getIndividualsAsList.asScala.map(individualToLF)
          val tp = ApplySpine(OWL2OMS(sig, dec), args.toSeq: _*)
          (null, tp)

        case ax: OWLPropertyAssertionAxiom[p, o] =>
          val tp = ax match {
            case ax: OWLObjectPropertyAssertionAxiom =>
              ApplySpine(OWL2OMS("OWL2SUB", "objectPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), individualToLF(ax.getObject))
            case ax: OWLNegativeObjectPropertyAssertionAxiom =>
              ApplySpine(OWL2OMS("OWL2ELRL", "negativeObjectPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), individualToLF(ax.getObject))
            case ax: OWLDataPropertyAssertionAxiom =>
              ApplySpine(OWL2OMS("OWL2SUB", "dataPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), literalToLF(ax.getObject))
            case ax: OWLNegativeDataPropertyAssertionAxiom =>
              ApplySpine(OWL2OMS("OWL2ELRL", "negativeDataPropertyAssertion"), propertyToLF(ax.getProperty), individualToLF(ax.getSubject), literalToLF(ax.getObject))
          }
          (null, tp)

        // AnnotationAxiom
        /* case ax : OWLAnnotationAssertionAxiom =>
              getAnnotation,
              val arg1 = ax.getProperty
              val arg2 = ax.getSubject // can either be IRIs or anonymous individuals
              val arg3 = ax.getValue //can either be an IRI (URI), Literal or Anonymous Individual

              val tp = ApplySpine(OWL2OMS("OWL2SUB", "annotationAssertionAxiom"), annotationPropertyToLF(arg1),?,annotationValueToLF(arg3))

              (null, tp)

              AnnotationAssertion( AP as av ) as: IRI or anonyind
              AnnotationAssertion( rdfs:label a:Person "Represents the set of all people." )

           case ax : OWLSubAnnotationPropertyOfAxiom =>
                val arg1 = ax.getSubProperty
              val arg2 = ax.getSuperProperty
              val tp = ApplySpine(OWL2OMS("OWL2SUB", "subAnnotationPropertyOf"), annotationPropertyToLF(arg1), annotationPropertyToLF(arg2))

                (null, tp)

          case ax : OWLAnnotationPropertyDomainAxiom =>
                val arg1 = ax.getProperty
                val arg2 = ax.getDomain (IRI veriyor)
                val tp = ApplySpine(OWL2OMS("OWL2SUB", "annotationPropertyDomain"), annotationPropertyToLF(arg1),?)

                (null, tp)

           case ax : OWLAnnotationPropertyRangeAxiom  =>
                val arg1 = ax.getProperty
                val arg2 = ax.getRange (IRI veriyor)
                val tp = ApplySpine(OWL2OMS("OWL2SUB", "annotationPropertyRange"), annotationPropertyToLF(arg1),?)

               (null, tp)
     */
        case _ => throw Exception("None of the axioms")
        //Apply(Apply(f,a),b),  ApplySpine(f,a,b)
      }

    val mDatum = ax.getAnnotations.asScala.map(annotationToLF)
    val mData = new MetaData()
    mData.add(mDatum.toList: _*) // list into a sequence
    addConstant(ax, name, tp, mData)
  }

  def annotationPropertyToLF(ap: OWLAnnotationProperty): GlobalName = {
    val apIRI = ap.getIRI
    val apString = apIRI.toString

    val base =
      if (ap.isBuiltIn) {
        apString match {
          case r if r == "http://www.w3.org/2000/01/rdf-schema#label" || r == "http://www.w3.org/2000/01/rdf-schema#comment" ||
            r == "http://www.w3.org/2000/01/rdf-schema#seeAlso" || r == "http://www.w3.org/2000/01/rdf-schema#isDefinedBy"
          => "http://www.w3.org/2000/01/rdf-schema"

          case o if o == "http://www.w3.org/2002/07/owl#deprecated" || o == "http://www.w3.org/2002/07/owl#priorVersion" ||
            o == "http://www.w3.org/2002/07/owl#backwardCompatibleWith" || o == "http://www.w3.org/2002/07/owl#incompatibleWith"
          => "http://www.w3.org/2002/07/owl"
          case _ => throw Exception("none of the built in annotation properties")
        }
      }
      else
        utils.URI(apIRI.toURI).copy(fragment = None).toString

    val dpath = DPath(utils.URI(base))
    val name = ap.getIRI.getFragment
    dpath ? "_" ? name //  global name = Dpath?String?String ,  base module name ,  rdfs_label

    /*
      boolean    containsAnnotationPropertyInSignature(IRI owlAnnotationPropertyIRI)
          Determines if the signature of this ontology contains an OWLAnnotationProperty with the specified IRI.
          boolean    containsAnnotationPropertyInSignature(IRI owlAnnotationPropertyIRI, boolean includeImportsClosure)
          Determines if the signature of this ontology, or possibly the signature of one of the ontologies in the imports closure of this ontology,
          contains an OWLAnnotationProperty that has the specified IRI

          boolean    isDeclared(OWLEntity owlEntity)
          Determines if this ontology declares an entity i.e.
          boolean    isDeclared(OWLEntity owlEntity, boolean includeImportsClosure)
          Determines if this ontology or its imports closure declares an entity i.e.
    */
  }

  def annotationValueToLF(av: OWLAnnotationValue): Term = {
    av match {
      case av: IRI => OMLiteral.URI(av.toURI)
      case av: OWLLiteral => literalToLF(av)
      case av: OWLAnonymousIndividual => individualToLF(av)
    }
  }

  def annotationToLF(an: OWLAnnotation): MetaDatum = {
    val key = annotationPropertyToLF(an.getProperty) //GlobalName
    val value = annotationValueToLF(an.getValue) //Obj or a term
    new MetaDatum(key, value)
  }
}

class OWLCompiler extends archives.Importer {
  val key = "owl-omdoc"

  def inExts = List("owl")

  def importDocument(bf: archives.BuildTask, seCont: documents.Document => Unit): BuildResult = {
    val source: File = bf.inFile
    val target: File = bf.outFile.setExtension("omdoc")

    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
    val importer = new Import(manager, controller)

    val ontology: OWLOntology = manager.loadOntologyFromOntologyDocument(source)
    /*println("Loaded ontology: " + ontology)*/

    //manager.getOntologies.foreach {onto => ontologyToLF(manager, controller, onto )} sor ?
    val dpath: DPath = importer.ontologyToLF(ontology)
    val doc = controller.getDocument(dpath)
    /* println(doc.toString)*/
    seCont(doc)
    BuildResult.empty
  }
}

object Import {
  def main(args: Array[String]): Unit = {
    /*val report = new FileReport(new java.io.File("controller.log")) //report("owl", "message")
    val checker = new FoundChecker(new DefaultFoundation, report)
    val controller = new Controller(checker, report)
    controller.handle(ExecFile(new java.io.File("startup.mmt")))
    val manager : OWLOntologyManager = OWLManager.createOWLOntologyManager()
    val importer = new Import (manager, controller)
    */

    //val owlcompiler = new OWLCompiler


    /*
      val source : File = new File(arg(0))
      val target : File = new File(arg(1))
    */
    //val file : File = new File("examples\\ex2.owl");


    /*      val source : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Axioms\\AssertionAxiom\\assertionAxiom.owl")
        val target : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Axioms\\AssertionAxiom\\assertionAxiom.omdoc")
    */

    /*
    val source : File = new File("C:\\Users\\toshiba\\Desktop\\OWLMMTYedek\\TestTogether\\Base\\base.owl")
    val target : File = new File("C:\\Users\\toshiba\\Desktop\\OWLMMTYedek\\TestTogether\\Base\\base2.omdoc")


    val ontology : OWLOntology  = manager.loadOntologyFromOntologyDocument(source)
    println("Loaded ontology: " + ontology)

    //manager.getOntologies.foreach {onto => ontologyToLF(manager, controller, onto )} sor ?
    val dpath : DPath = importer.ontologyToLF(ontology)
    val doc = controller.getDocument(dpath).toNodeResolved(controller.library)
    println(doc.toString)

    val file = new FileWriter(target)
    file.write(doc.toString)
    file.close
    */
  }
}
