package io.circe.bson

import io.circe.{ Json, JsonNumber }
import io.circe.testing.ArbitraryInstances
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import reactivemongo.bson.BSONDecimal
import scala.util.{ Failure, Success, Try }

class BsonCodecInstancesTest extends FunSuite with GeneratorDrivenPropertyChecks with ArbitraryInstances {
  /**
   * Note that we zero out JSON number values whose string representation can't
   * be parsed by `BSONDecimal`.
   */
  override def transformJsonNumber(n: JsonNumber): JsonNumber =
    Try(BSONDecimal.parse(n.toString)).flatten match {
      case Success(_) => n
      case Failure(_) => JsonNumber.fromString("0").get
    }

  test("BsonCodecInstances should round-trip JSON values") {
    forAll { json: Json =>
      assert(Right(json) === jsonToBson(json).right.flatMap(bsonToJson))
    }
  }
}
