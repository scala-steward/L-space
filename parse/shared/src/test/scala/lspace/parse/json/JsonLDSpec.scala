package lspace.parse.json

import java.time.Instant

import argonaut._
import lspace.NS.types
import lspace.librarian.datatype.ListType
import lspace.librarian.process.traversal.step.HasLabel
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.{MemGraph, MemGraphDefault}
import lspace.librarian.structure._
import lspace.librarian.util.SampleGraph
import lspace.parse.util.{HttpClient, HttpClientImpl}
import org.scalatest.OptionValues._
import org.scalatest.{Matchers, WordSpec}

import scala.util.{Failure, Success}

class JsonLDSpec extends WordSpec with Matchers {
  val graph  = MemGraph("JsonLDSpec")
  val jsonld = JsonLD(graph)
  SampleGraph.loadSocial(graph)

  val httpClient: HttpClient = HttpClientImpl

  "A JsonLDParser" should {
    "parse and encode literals" in {
      val json5 = jsonld.anyToJson(5L, List(DataType.default.`@long`))._1
      json5.toString() shouldBe """"5""""
      jsonld.jsonToValue(DataType.default.`@long`, json5).map(_._2) shouldBe Success(5L)
      val json5object = jsonld.anyToJson(5L)._1
      json5object.toString() shouldBe """{"@value":"5","@type":"@long"}"""
      Parse.parse(""""5"""").right.get.string.isDefined shouldBe true
      Parse.parse(""""5"""").right.get.string.get.toLong shouldBe 5L
      //      Parse.parse("""""5"""").right.get.number.isDefined shouldBe true
    }
    "parse a traversal to json" in {
      val traversal       = MemGraphDefault.g.N.has(SampleGraph.properties.name) //, Some(Traversal.ontology))
      val (json, builder) = jsonld.nodeToJsonWithContext(traversal.self)
      json.obj.get ?? types.`@context` shouldBe true
      val nodeTry = jsonld.resource(json.obj.get).filter(_.isInstanceOf[Node]).map(_.asInstanceOf[Node])
      nodeTry match {
        case Success(r) =>
        case Failure(e) => println(e.getMessage)
      }
      nodeTry.isSuccess shouldBe true
      val node              = nodeTry.get
      val parsedTraversal   = Traversal.wrap(node)(MemGraphDefault)
      val (json2, builder2) = jsonld.nodeToJsonWithContext(parsedTraversal.self)
      val nodeTry2          = jsonld.resource(json2.obj.get).filter(_.isInstanceOf[Node]).map(_.asInstanceOf[Node])
      nodeTry2.isSuccess shouldBe true
      val node2             = nodeTry2.get
      val parsedTraversal2  = Traversal.wrap(node2)(MemGraphDefault)
      val (json3, builder3) = jsonld.nodeToJsonWithContext(parsedTraversal2.self)
      json shouldBe json2
      json shouldBe json2
      json shouldBe json3
      builder.context shouldBe builder2.context
      builder2.context shouldBe builder3.context
      builder.iriSet shouldBe builder2.iriSet
      builder2.iriSet shouldBe builder3.iriSet
    }

    "parse an ontology to json" in {
      val name      = Property("thing/name")(_range = () => DataType.default.`@string` :: Nil)
      val typedName = name + DataType.default.`@string`
      val surname =
        Property("thing/surname")(_range = () => DataType.default.`@string` :: Nil, containers = List(types.`@set`))
      val typedSurname = surname + DataType.default.`@string`
      val testOntology: Ontology =
        Ontology("thing")(_properties = () => name :: surname :: Nil,
                          _extendedClasses = () => new Ontology("basething") {} :: Nil)

      val json = jsonld.ontologyToJson(testOntology)._1
      json ?? types.`@id` shouldBe true
      json(types.`@id`).exists(_.string.contains("thing")) shouldBe true

      json ?? types.`@properties` shouldBe true
      json(types.`@properties`).map(_.isArray).value shouldBe true
      json(types.`@properties`).get.array.get.exists(_.string.contains("thing/name")) shouldBe true
      json(types.`@properties`).get.array.get.exists(_.string.contains("thing/surname")) shouldBe true
    }

    val baseOntology: Ontology = Ontology("basething")

    MemGraphDefault.ns.storeOntology(baseOntology)
    //    baseOntology.status := CacheStatus.CACHED
    val name      = Property("thing/name")(_range = () => DataType.default.`@string` :: Nil)
    val typedName = name + DataType.default.`@string`
    val surname =
      Property("thing/surname")(_range = () => DataType.default.`@string` :: Nil, containers = List(types.`@set`))
    val typedSurname = surname + DataType.default.`@string`
    val testOntology: Ontology =
      Ontology("thing")(_properties = () => name :: surname :: Nil, _extendedClasses = () => baseOntology :: Nil)

    val propertyJsonName = jsonld.propertyToJson(name)
    MemGraphDefault.ns.storeProperty(name)
    //    name.status := CacheStatus.CACHED
    val propertyJsonSurname = jsonld.propertyToJson(surname)
    MemGraphDefault.ns.storeProperty(surname)
    //    surname.status := CacheStatus.CACHED
    val ontologyJson = jsonld.ontologyToJson(testOntology)._1
    val ontologyNode = MemGraphDefault.ns.storeOntology(testOntology)

    "parse json to an ontology" in {
      MemGraphDefault.ns.getOntology(testOntology.iri).isDefined shouldBe true
      val ontology = jsonld.resource(ontologyJson)
      ontology shouldBe Success(ontologyNode)
    }

    "parse a node to json" in {
      val node = DetachedGraph.nodes.create()
      node.addOut(Property.default.typed.iriUrlString, "abc")
      node.addLabel(testOntology)
      node.addOut(Property.default.typed.irisUrlString, "def")
      node.addOut(typedName, "Alice")
      node.addOut(typedSurname, "A")
      node.addOut(typedSurname, "B")
      val json = jsonld.nodeToJson(node)._1
      //      println(json.toString())
      json ?? types.`@id` shouldBe true
      json(types.`@id`).get.string.contains("abc") shouldBe true

      json ?? types.`@type` shouldBe true
      json(types.`@type`).get.string.contains("thing") shouldBe true

      json ?? "thing/name" shouldBe true
      json("thing/name").get.string should contain("Alice")

      json ?? "thing/surname" shouldBe true
      json("thing/surname").get.arrayOrEmpty.flatMap(_.string) should contain("A")
      val surnamesSet = json("thing/surname").get.array
        .map { values =>
          values.map { json =>
            json.string.getOrElse(null)
          }
        }
        .getOrElse(throw new Exception("??"))
        .toSet
      surnamesSet should contain allOf ("A", "B")
      surnamesSet.size shouldBe 2
      surnamesSet shouldNot contain("C")
    }

    "parse json to a node" in {
      val node = DetachedGraph.nodes.create()
      node.addOut(Property.default.typed.iriUrlString, "abc")
      node.addLabel(testOntology)
      node.addOut(Property.default.typed.irisUrlString, "def")
      node.addOut(typedName, "Alice")
      node --- surname --> "A"
      node --- surname --> "B"
      val json = jsonld.nodeToJson(node)._1
      //      println(Json.jObject(json).toString)
      val parsedResource = jsonld.resource(json)
      parsedResource.map(_.iri) shouldBe Success(node.iri)

      val parsedNode = parsedResource.filter(_.isInstanceOf[Node]).map(_.asInstanceOf[Node])

      parsedNode.get.out(name).size shouldBe 1
      parsedNode.get.out(surname).size shouldBe 2
      parsedNode.get.out(surname).size shouldBe 2
      parsedNode.get.out(surname).size shouldBe 2
      parsedNode.get.out(surname) should contain allOf ("A", "B")
    }

    "parse a list of nodes" in {
      val result = MemGraphDefault.g.N().range(80, 82).toList
      //      println(result.map(_.asInstanceOf[MemResource[_]].id))
      val json = jsonld.anyToJson(result)._1
      //      println(Json.prettyPrint(json))
      //      val parsedResult = jsonParser.list(json)
      //      parsedResult.isSuccess shouldBe true
      //      println(parsedResult.get.map {
      //        case node: Node => node.value.asInstanceOf[MemResource[_]].id
      //        case value => value.toString
      //      })
      //      val json2 = jsonParser.anyListToJson(parsedResult.get)._1
      //      println(Json.prettyPrint(json2))
    }

    "parse a property from schema.org" in {
      val result = httpClient
        .getResource("https://schema.org/propertyID") { json =>
          jsonld.jsonToProperty(Parse.parseOption(json).get.obj.get)
        }
        .flatten
      result match {
        case Success(s) =>
        case Failure(e) =>
          e.printStackTrace()
          println(e.getMessage())
      }
      result.isSuccess shouldBe true
      val resultname = httpClient
        .getResource("https://schema.org/name") { json =>
          jsonld.jsonToProperty(Parse.parseOption(json).get.obj.get)
        }
        .flatten
      resultname match {
        case Success(s) => s
        case Failure(e) =>
          e.printStackTrace()
          println(e.getMessage())
      }
      resultname.isSuccess shouldBe true
    }

    "parse an OutMap-/InMap-result list to objects in json" in {
      val traversal       = graph.g.N().has(SampleGraph.properties.balance, P.gt(5.0)).outMap()
      val dt              = traversal.ct //(new DataType[Any] { val iri = "" })
      val collection      = Collection(Instant.now(), Instant.now(), traversal.toList)(dt)
      val (json, context) = jsonld.nodeToJsonWithContext(collection)

//      println(json)

      val collectionNode = jsonld.resource(json.obj.get) match {
        case Success(r) =>
          r.asInstanceOf[Node]
        case Failure(e) =>
          println(e.getMessage)
          throw e
      }
      Collection.apply(collectionNode, dt).item.size shouldBe 4
    }

    "parse an OutEMap-/InEMap-result list to objects in json" in {
      val traversal       = graph.g.N().has(SampleGraph.properties.balance, P.gt(5.0)).outEMap()
      val dt              = traversal.ct //(new IriType[Edge[Node, Any]] { val iri = "" })
      val collection      = Collection(Instant.now(), Instant.now(), traversal.toList)(dt)
      val (json, context) = jsonld.nodeToJsonWithContext(collection)
      //      println(json)

      val collectionNode = jsonld.resource(json.obj.get) match {
        case Success(r) => r.asInstanceOf[Node]
        case Failure(e) =>
          println(e.getMessage)
          throw e
      }
      Collection.apply(collectionNode, dt).item.size shouldBe 4
      Collection.apply(collectionNode, dt).item.head.size shouldBe 7
    }

    "parse a group-result list to objects in json" in {
      val traversal =
        graph.g.N().has(SampleGraph.properties.balance, P.gt(5.0)).group(_.out(SampleGraph.properties.balance))
      val dt              = traversal.ct
      val collection      = Collection(Instant.now(), Instant.now(), traversal.toList)(dt)
      val (json, context) = jsonld.nodeToJsonWithContext(collection)
      //      println(json)

      val collectionNode = jsonld.resource(json.obj.get) match {
        case Success(r) => r.asInstanceOf[Node]
        case Failure(e) =>
          println(e.getMessage)
          throw e
      }
      Collection.apply(collectionNode, dt).item.size shouldBe 1
      Collection.apply(collectionNode, dt).item.head.size shouldBe 4
    }

    "parse an any-result list" in {
      val traversal = graph.g.N().has(SampleGraph.properties.balance, P.gt(5.0)).out(SampleGraph.properties.balance)
      //      val dt: ClassType[Any] = traversal.ct //FIX: no type-resolve for dt: 'Cannot be cast to scala.runtime.Nothing'
      val collection      = Collection(Instant.now(), Instant.now(), traversal.toList)(traversal.ct)
      val (json, context) = jsonld.nodeToJsonWithContext(collection)
      //      println(json)

      val collectionNode = jsonld.resource(json.obj.get) match {
        case Success(r) => r.asInstanceOf[Node]
        case Failure(e) =>
          println(e.getMessage)
          throw e
      }
      Collection.apply(collectionNode, traversal.ct).item.size shouldBe 4
    }
  }
}
