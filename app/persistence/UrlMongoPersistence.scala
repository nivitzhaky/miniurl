package persistence

import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import persistence.MongoObject.client
import space.Bookmark
import sun.security.util.Length
import utils.MongoUtils

import scala.annotation.tailrec
import scala.util.Try

object MongoObject {
  lazy val client = MongoClient()

}
object DBName {
  val miniurl = "miniurl"
}
object TableName {
  val urls = "urls"
}

case class MongoUrl(_id: String)
case class MongoBookmark(_id: String, url: String, alias: String, clicks: Int, user: Option[String] = None)

class UrlMongoPersistence(dbName: String) extends MongoUtils {

  import TableName._
  lazy val db = client.getDB(dbName)

  def MDB = MongoDBObject
  def addMiniUrlId(url: MongoUrl) = {
    db(urls).insert(toDBObj(url))
  }
  def TakeMiniUrlById(miniurl: String, bookmark: Bookmark) = {
    db(urls).update(MDB("_id" -> miniurl), MDB("$set" -> bookmark.getAsMongoFields()))
  }

  def getUrlByMini(miniurl: String, incClicks: Boolean = false): Bookmark = {
    println("getbymini:" + miniurl + "->" + dbName)
    val res = if (incClicks) {
      db(urls).findAndModify(MDB("_id" -> miniurl), MDB("_id" -> 0), MDB("$inc" -> MDB("clicks" -> 1))).map(x => dbObjTo[Bookmark](x)).head
    } else {
      db(urls).findOne(MDB("_id" -> miniurl), MDB("_id" -> 0)).map(x => dbObjTo[Bookmark](x)).get
    }
    println(res)
    res
  }
  def getUrlByUser(user: String) = {
    val res = db(urls).find(MDB("user" -> user)).map(x => dbObjTo[MongoBookmark](x)).toList
    println(res)
    res
  }

  def getUsedUrls(user: Option[String]): List[MongoBookmark] = {
    val filter = List("url" -> MDB("$exists" -> true)) ++ user.map(x => "user" -> x).toList
    val res = db(urls).find(MDB(filter)).map(x => dbObjTo[MongoBookmark](x)).toList
    println(res)
    res
  }
  def deleteMiniUrl(miniurl: String) = {
    db(urls).remove(MDB("_id" -> miniurl))
  }

  def createIndexes() = {
    db(urls).createIndex(MDB("url" -> 1, "user" -> 2))
  }

}

case class MiniUrlGen(db: UrlMongoPersistence, length: Integer = 6) {
  val allowed = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
  val r = scala.util.Random
  var totalTimeInside = 0L
  def genUrl(): String = {

    tryXTimes({
      val key = (1 to length).map(x => allowed(r.nextInt(allowed.size))).mkString
      val t = System.currentTimeMillis();
      db.addMiniUrlId(MongoUrl(key))
      totalTimeInside += (System.currentTimeMillis() - t)
      key
    }, times = 10)

  }

  def tryXTimes(f: => String, times: Integer): String = {
    if (times == 0) throw new RuntimeException("Unable to create a key")
    Try { f }.recover {
      case e: Throwable =>
        println(e.getMessage)
        tryXTimes(f, times - 1)
    }.get
  }
}

object testPersist extends App {
  val db = new UrlMongoPersistence(DBName.miniurl)
  //  val gen = MiniUrlGen(db);
  //
  //  (1 to 1).foreach(x => gen.genUrl())
  //  println(gen.totalTimeInside)
  println(db.getUrlByMini("UzKVZT"))

}