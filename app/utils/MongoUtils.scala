package utils

import com.mongodb.DBObject
import org.json4s.mongo.JObjectParser
import org.json4s.{ DefaultFormats, Extraction }

import scala.util.Try

trait MongoUtils {

  implicit val formats = new DefaultFormats {}

  protected def dbObjTo[A](from: DBObject)(implicit manifest: Manifest[A]): A = {
    val jValue = JObjectParser.serialize(from)
    val entity: A = Extraction.extract[A](jValue)
    entity
  }

  case class ID(_id: String)
  protected def toDBObj(any: Any): DBObject = {
    val json = Extraction.decompose(any)
    val parsed = JObjectParser.parse(json)
    parsed
  }
}

trait Eventually {
  def eventually(f: => Boolean, errorMessage: String)(times: Integer, interval: Long): Unit = {
    if (times == 0) throw new RuntimeException(errorMessage)
    val res = f
    if (!(res)) {
      Thread.sleep(interval)
      eventually(f, errorMessage)(times - 1, interval)
    }
  }
}