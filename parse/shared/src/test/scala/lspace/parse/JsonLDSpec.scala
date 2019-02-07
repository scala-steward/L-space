package lspace.parse

import argonaut._
import lspace.NS.types
import lspace.librarian.datatype.DataType
import lspace.librarian.process.traversal._
import lspace.librarian.provider.mem.{MemGraph, MemGraphDefault}
import lspace.librarian.structure._
import lspace.librarian.util.SampleGraph
import lspace.parse.util.{HttpClient, HttpClientImpl}
import lspace.parse.JsonObjectInProgress._
import monix.execution.Scheduler
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.ExecutionContextExecutor

class JsonLDSpec extends AsyncWordSpec with Matchers {
  val graph       = MemGraph("JsonLDSpec")
  val decoder     = lspace.codec.argonaut.Decode(graph)
  val encoder     = lspace.codec.argonaut.Encode
  implicit val ac = ActiveContext()
  implicit val ec = Scheduler.apply(new ExecutionContextExecutor {

    def execute(runnable: Runnable): Unit = {
      try {
        runnable.run()
      } catch {
        case t: Throwable => reportFailure(t)
      }
    }

    def reportFailure(t: Throwable): Unit =
      t.printStackTrace()
  })

  SampleGraph.loadSocial(graph)

  val httpClient: HttpClient = HttpClientImpl

  "A JsonLDParser" should {
    "parse and encode literals" in {
      val json5 = encoder.fromAny(5L, Some(DataType.default.`@long`)).json
      json5.toString() shouldBe """5"""

      decoder.toData(json5, DataType.default.`@long`).runToFuture.map { v =>
        v shouldBe 5L
        val json5object = encoder.fromAny(5L)
        json5object.json.toString() shouldBe """{"@value":5,"@type":"@long"}"""
        Parse.parse(""""5"""").right.get.string.isDefined shouldBe true
        Parse.parse(""""5"""").right.get.string.get.toLong shouldBe 5L
      //      Parse.parse("""""5"""").right.get.number.isDefined shouldBe true
      }
    }
    "parse a traversal to json" in {
      val traversal = MemGraphDefault.g.N.has(SampleGraph.properties.name) //, Some(Traversal.ontology))
      val jip       = encoder.fromNode(traversal.toNode)
      encoder.fromNode(traversal.toNode).withContext ?? types.`@context` shouldBe true
      decoder
        .stringToNode(encoder(traversal.toNode))
        .runToFuture
        .flatMap { node =>
          val parsedTraversal = Traversal.toTraversal(node)(MemGraphDefault)
          val jip2            = encoder.fromNode(parsedTraversal.toNode)
          decoder.stringToLabeledNode(encoder(parsedTraversal.toNode), Traversal.ontology).runToFuture.map { node2 =>
            val parsedTraversal2 = Traversal.toTraversal(node2)(MemGraphDefault)
            val jip3             = encoder.fromNode(parsedTraversal2.toNode)
            jip.json shouldBe jip2.json
            jip.json shouldBe jip2.json
            jip.json shouldBe jip3.json
            jip.activeContext shouldBe jip2.activeContext
            jip2.activeContext shouldBe jip3.activeContext
          }
        }
    }

    "parse an ontology to json" in {
      val name      = Property("thing/name", range = DataType.default.`@string` :: Nil)
      val typedName = name + DataType.default.`@string`
      val surname =
        Property("thing/surname", range = DataType.default.`@string` :: Nil, containers = List(types.`@set`))
      val typedSurname = surname + DataType.default.`@string`
      val testOntology: Ontology =
        Ontology("thing", properties = name :: surname :: Nil, extendedClasses = new Ontology("basething") {} :: Nil)

      val jip  = encoder.fromOntology(testOntology)
      val json = jip.json
      json ?? types.`@id` shouldBe true
      json(types.`@id`).exists(_.string.contains("thing")) shouldBe true

      json ?? types.`@properties` shouldBe true
      json(types.`@properties`).get.isArray shouldBe true
      json(types.`@properties`).get.array.get.exists(_.string.contains("thing/name")) shouldBe true
      json(types.`@properties`).get.array.get.exists(_.string.contains("thing/surname")) shouldBe true
    }

    val baseOntology: Ontology = Ontology("basething")

    MemGraphDefault.ns.ontologies.store(baseOntology)
    //    baseOntology.status := CacheStatus.CACHED
    val name      = Property("thing/name", range = DataType.default.`@string` :: Nil)
    val typedName = name + DataType.default.`@string`
    val surname =
      Property("thing/surname", range = DataType.default.`@string` :: Nil, containers = List(types.`@set`))
    val typedSurname = surname + DataType.default.`@string`
    val testOntology: Ontology =
      Ontology("thing", properties = name :: surname :: Nil, extendedClasses = baseOntology :: Nil)

    val propertyJsonName = encoder.fromProperty(name)
    MemGraphDefault.ns.properties.store(name)
    //    name.status := CacheStatus.CACHED
    val propertyJsonSurname = encoder.fromProperty(surname)
    MemGraphDefault.ns.properties.store(surname)
    //    surname.status := CacheStatus.CACHED
    val ontologyJson = encoder.fromOntology(testOntology)
    val ontologyNode = MemGraphDefault.ns.ontologies.store(testOntology)

    "parse json to an ontology" in {
      MemGraphDefault.ns.ontologies.get(testOntology.iri).isDefined shouldBe true
      decoder.toOntology(ontologyJson.json.toMap)(ontologyJson.activeContext).runToFuture.map { ontology =>
        ontology shouldBe testOntology
      }
    }
//
//    "parse a node to json" in {
//      val node = DetachedGraph.nodes.create()
//      node.addOut(Property.default.typed.iriUrlString, "abc")
//      node.addLabel(testOntology)
//      node.addOut(Property.default.typed.irisUrlString, "def")
//      node.addOut(typedName, "Alice")
//      node.addOut(typedSurname, "A")
//      node.addOut(typedSurname, "B")
//      val json = jsonld.nodeToJsonObject(node)._1
//      //      println(json.toString())
//      json ?? types.`@id` shouldBe true
//      json(types.`@id`).get.string.contains("abc") shouldBe true
//
//      json ?? types.`@type` shouldBe true
//      json(types.`@type`).get.string.contains("thing") shouldBe true
//
//      json ?? "thing/name" shouldBe true
//      json("thing/name").get.string should contain("Alice")
//
//      json ?? "thing/surname" shouldBe true
//      json("thing/surname").get.arrayOrEmpty.flatMap(_.string) should contain("A")
//      val surnamesSet = json("thing/surname").get.array
//        .map { values =>
//          values.map { json =>
//            json.string.getOrElse(null)
//          }
//        }
//        .getOrElse(throw new Exception("??"))
//        .toSet
//      surnamesSet should contain allOf ("A", "B")
//      surnamesSet.size shouldBe 2
//      surnamesSet shouldNot contain("C")
//    }
//
//    "parse json to a node" in {
//      val node = DetachedGraph.nodes.create()
//      node.addOut(Property.default.typed.iriUrlString, "abc")
//      node.addLabel(testOntology)
//      node.addOut(Property.default.typed.irisUrlString, "def")
//      node.addOut(typedName, "Alice")
//      node --- surname --> "A"
//      node --- surname --> "B"
//      val json = jsonld.nodeToJsonObject(node)._1
//      //      println(Json.jObject(json).toString)
//      val parsedResource = jsonld.resource(json)
//      parsedResource.map(_.iri) shouldBe Success(node.iri)
//
//      val parsedNode = parsedResource.filter(_.isInstanceOf[Node]).map(_.asInstanceOf[Node])
//
//      parsedNode.get.out(name).size shouldBe 1
//      parsedNode.get.out(surname).size shouldBe 2
//      parsedNode.get.out(surname).size shouldBe 2
//      parsedNode.get.out(surname).size shouldBe 2
//      parsedNode.get.out(surname) should contain allOf ("A", "B")
//    }
//
//    "parse a list of nodes" in {
//      val result = MemGraphDefault.g.N().range(80, 82).toList
//      //      println(result.map(_.asInstanceOf[MemResource[_]].id))
//      val json = jsonld.anyToJson(result)._1
//      //      println(Json.prettyPrint(json))
//      //      val parsedResult = jsonParser.list(json)
//      //      parsedResult.isSuccess shouldBe true
//      //      println(parsedResult.get.map {
//      //        case node: Node => node.value.asInstanceOf[MemResource[_]].id
//      //        case value => value.toString
//      //      })
//      //      val json2 = jsonParser.anyListToJson(parsedResult.get)._1
//      //      println(Json.prettyPrint(json2))
//    }
//
//    "parse a property from schema.org" in {
//      val result = httpClient
//        .getResource("https://schema.org/propertyID") { json =>
//          jsonld.jsonToProperty(Parse.parseOption(json).get.obj.get)
//        }
//        .flatten
//      result match {
//        case Success(s) =>
//        case Failure(e) =>
//          e.printStackTrace()
//          println("error: " + e.getMessage())
//      }
//      result.isSuccess shouldBe true
//      val resultname = httpClient
//        .getResource("https://schema.org/name") { json =>
//          jsonld.jsonToProperty(Parse.parseOption(json).get.obj.get)
//        }
//        .flatten
//      resultname match {
//        case Success(s) => s
//        case Failure(e) =>
//          e.printStackTrace()
//          println("error: " + e.getMessage())
//      }
//      resultname.isSuccess shouldBe true
//    }
//
//    "parse an OutMap-/InMap-result list to objects in json" in {
//      val traversal       = graph.g.N().has(SampleGraph.properties.balance, P.gt(5.0)).outMap()
//      val dt              = traversal.ct //(new DataType[Any] { val iri = "" })
//      val collection      = Collection(Instant.now(), Instant.now(), traversal.toList, dt)
//      val (json, context) = jsonld.nodeToJsonWithContext(collection)
//
////      println(traversal.toList)
////      println(collection.item)
////      println(json)
//
//      val collectionNode = jsonld.resource(json.obj.get) match {
//        case Success(r) =>
//          r.asInstanceOf[Node]
//        case Failure(e) =>
//          println("error: " + e.getMessage)
//          throw e
//      }
//
//      Collection.apply(collectionNode, dt).item.size shouldBe 4
//    }
//
//    "parse an OutEMap-/InEMap-result list to objects in json" in {
//      val traversal       = graph.g.N().has(SampleGraph.properties.balance, P.gt(5.0)).outEMap()
//      val dt              = traversal.ct //(new IriType[Edge[Node, Any]] { val iri = "" })
//      val collection      = Collection(Instant.now(), Instant.now(), traversal.toList, dt)
//      val (json, context) = jsonld.nodeToJsonWithContext(collection)
//
////      println(traversal.toList)
////      println(collection.item)
////      println(json)
//
//      val collectionNode = jsonld.resource(json.obj.get) match {
//        case Success(r) => r.asInstanceOf[Node]
//        case Failure(e) =>
//          println("error: " + e.getMessage)
//          throw e
//      }
//      Collection.apply(collectionNode, dt).item.size shouldBe 4
//      Collection.apply(collectionNode, dt).item.head.size shouldBe 7
//    }
//
//    "parse a group-result list to objects in json" in {
//      val traversal =
//        graph.g.N().has(SampleGraph.properties.balance, P.gt(5.0)).group(_.out(SampleGraph.properties.balance))
//      val dt              = traversal.ct
//      val collection      = Collection(Instant.now(), Instant.now(), traversal.toList, dt)
//      val (json, context) = jsonld.nodeToJsonWithContext(collection)
//      //      println(json)
//
//      val collectionNode = jsonld.resource(json.obj.get) match {
//        case Success(r) => r.asInstanceOf[Node]
//        case Failure(e) =>
//          println("error: " + e.getMessage)
//          throw e
//      }
//      Collection.apply(collectionNode, dt).item.size shouldBe 1
//      Collection.apply(collectionNode, dt).item.head.size shouldBe 4
//    }
//
//    "parse an any-result list" in {
//      val traversal = graph.g.N().has(SampleGraph.properties.balance, P.gt(5.0)).out(SampleGraph.properties.balance)
//      //      val dt: ClassType[Any] = traversal.ct //FIX: no type-resolve for dt: 'Cannot be cast to scala.runtime.Nothing'
//      val collection      = Collection(Instant.now(), Instant.now(), traversal.toList, traversal.ct)
//      val (json, context) = jsonld.nodeToJsonWithContext(collection)
//      //      println(json)
//
//      val collectionNode = jsonld.resource(json.obj.get) match {
//        case Success(r) => r.asInstanceOf[Node]
//        case Failure(e) =>
//          println("error: " + e.getMessage)
//          throw e
//      }
//      Collection.apply(collectionNode, traversal.ct).item.size shouldBe 4
//    }
  }
}
