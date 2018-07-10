package io.circe.bson

import io.circe.Json
import io.circe.testing.instances._
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import reactivemongo.bson.BSONDecimal

class BsonCodecInstancesTest extends FunSuite with GeneratorDrivenPropertyChecks {
  private def badJsonNumber(json: Json): Boolean = json.fold(
    false,
    _ => false,
    n => BSONDecimal.parse(n.toString).isFailure,
    _ => false,
    v => v.exists(badJsonNumber),
    o => o.toVector.map(_._2).exists(badJsonNumber)
  )

  test("BsonCodecInstances should round-trip JSON values") {
    forAll { (json: Json) =>
      val converted = jsonToBson(json).right.flatMap(bsonToJson)

      whenever(!badJsonNumber(json)) {
        assert(Right(json) === converted)
      }
    }
  }
}
