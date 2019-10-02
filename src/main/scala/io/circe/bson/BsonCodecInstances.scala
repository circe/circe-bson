package io.circe.bson

import cats.Traverse
import cats.instances.either._
import cats.instances.vector._
import io.circe.Json.JObject
import io.circe.{ Json, JsonNumber, JsonObject }
import reactivemongo.bson._
import reactivemongo.bson.exceptions.TypeDoesNotMatch
import scala.util.{ Failure, Success, Try }

trait BsonCodecInstances {
  private[this] def readerFailure(value: BSONValue): TypeDoesNotMatch =
    TypeDoesNotMatch(
      s"Cannot convert $value: ${value.getClass} to io.circe.Json with io.circe.bson"
    )

  // Cats no longer provides a `Traverse[Stream]` instance.
  private def traverseTryStream[A, B](s: Stream[Try[A]])(f: A => Either[Throwable, B]): Either[Throwable, Stream[B]] = {
    val builder = Stream.newBuilder[B]
    val iterator = s.iterator

    while (iterator.hasNext) {
      iterator.next match {
        case Success(a) =>
          f(a) match {
            case Right(b) => builder += b
            case Left(e)  => return Left(e)
          }
        case Failure(e) => return Left(e)
      }
    }

    Right(builder.result())
  }

  final def bsonToJson(bson: BSONValue): Either[Throwable, Json] = bson match {
    case BSONBoolean(value) => Right(Json.fromBoolean(value))
    case BSONString(value)  => Right(Json.fromString(value))
    case BSONDouble(value) =>
      Json.fromDouble(value) match {
        case Some(json) => Right(json)
        case None       => Left(readerFailure(bson))
      }
    case BSONLong(value)    => Right(Json.fromLong(value))
    case BSONInteger(value) => Right(Json.fromInt(value))
    case dec: BSONDecimal =>
      if (dec == BSONDecimal.NegativeZero) {
        Json.fromDouble(-0.0) match {
          case Some(json) => Right(json)
          case None       => Left(readerFailure(bson))
        }
      } else {
        BSONDecimal.toBigDecimal(dec) match {
          case Success(value) => Right(Json.fromBigDecimal(value))
          case Failure(error) => Left(error)
        }
      }
    case BSONArray(values) => traverseTryStream(values)(bsonToJson).map(Json.fromValues)
    case BSONDocument(values) =>
      traverseTryStream(values) {
        case BSONElement(key, value) => bsonToJson(value).map(key -> _)
      }.map(Json.fromFields)
    case BSONDateTime(value)     => Right(Json.obj("$date" ->Json.fromLong(value)))
    case BSONTimestamp(value)    => Right(Json.fromLong(value))
    case BSONNull                => Right(Json.Null)
    case BSONUndefined           => Right(Json.Null)
    case BSONSymbol(value)       => Right(Json.fromString(value))
    case BSONJavaScript(value)   => Right(Json.fromString(value))
    case BSONJavaScriptWS(value) => Right(Json.fromString(value))
    case BSONMaxKey              => Left(readerFailure(bson))
    case BSONMinKey              => Left(readerFailure(bson))
    case id: BSONObjectID =>
      BSONObjectID.parse(id.stringify) match {
        case Success(value) => Right(Json.fromString(value.stringify))
        case Failure(error) => Left(error)
      }
    case BSONBinary(_, _)    => Left(readerFailure(bson))
    case BSONDBPointer(_, _) => Left(readerFailure(bson))
    case BSONRegex(_, _)     => Left(readerFailure(bson))
  }

  private[this] lazy val jsonFolder: Json.Folder[Either[Throwable, BSONValue]] =
    new Json.Folder[Either[Throwable, BSONValue]] { self =>
      final val onNull: Either[Throwable, BSONValue] = Right(BSONNull)
      final def onBoolean(value: Boolean): Either[Throwable, BSONValue] = Right(BSONBoolean(value))
      final def onNumber(value: JsonNumber): Either[Throwable, BSONValue] = {
        val asDouble = value.toDouble

        if (java.lang.Double.compare(asDouble, -0.0) == 0) {
          Right(BSONDecimal.NegativeZero)
        } else
          value.toLong match {
            case Some(n) => Right(BSONLong(n))
            case None =>
              value.toBigDecimal match {
                case Some(n) =>
                  BSONDecimal.fromBigDecimal(n) match {
                    case Success(dec)   => Right(dec)
                    case Failure(error) => Left(error)
                  }
                case None =>
                  BSONDecimal.parse(value.toString) match {
                    case Success(dec)   => Right(dec)
                    case Failure(error) => Left(error)
                  }
              }
          }
      }
      final def onString(value: String): Either[Throwable, BSONValue] = Right(BSONString(value))
      final def onArray(value: Vector[Json]): Either[Throwable, BSONValue] =
        Traverse[Vector]
          .traverse(value) { json =>
            json.foldWith(self)
          }
          .map(BSONArray(_))
      final def onObject(value: JsonObject): Either[Throwable, BSONValue] =
        Traverse[Vector]
          .traverse(value.toVector) {
            case (key, json: JObject) if json.value.contains("$date") =>
              json.value("$date").flatMap(_.asNumber).flatMap(_.toLong).map(key -> BSONDateTime(_)) match {
                case Some(bdt) => Right(bdt)
                case None =>
                  Left(
                    TypeDoesNotMatch("Unable to convert JsonObject with $date to BSONDateTime, for key: %s".format(key))
                  )
              }
            case (key, json) => json.foldWith(self).map(key -> _)
          }
          .map(BSONDocument(_))
    }

  final def jsonToBson(json: Json): Either[Throwable, BSONValue] = json.foldWith(jsonFolder)

  implicit final lazy val jsonBsonReader: BSONReader[BSONValue, Json] = new BSONReader[BSONValue, Json] {
    final def read(bson: BSONValue): Json = bsonToJson(bson) match {
      case Right(value) => value
      case Left(error)  => throw error
    }
  }

  implicit final lazy val jsonBsonWriter: BSONWriter[Json, BSONValue] =
    new BSONWriter[Json, BSONValue] {
      def write(json: Json): BSONValue = jsonToBson(json) match {
        case Right(value) => value
        case Left(error)  => throw error
      }
    }
}
