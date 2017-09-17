package space

import java.net.URL

import akka.actor.{ Actor, Props }
import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject
import persistence.{ MiniUrlGen, UrlMongoPersistence }
import play.api.libs.ws.WS
import play.mvc.Http
import play.api.Play.current

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

case class Bookmark(url: String, alias: String, clicks: Integer = 0, user: Option[String] = None) {
  def getUrl() = {
    if (url.startsWith("http")) {
      url
    } else {
      "http://" + url
    }
  }
  def validate(): Try[Bookmark] = {
    Try {
      new URL(getUrl())
      Await.result(WS.url(getUrl()).get(), Duration.Inf)
    }.map(x => this).recover {
      case e: Throwable =>
        throw new RuntimeException("Invalid URL or unreachable:" + url)
    }
  }
  def getAsMongoFields(): DBObject = {
    val fields = List("url" -> url, "alias" -> alias, "clicks" -> clicks, "timestamp" -> System.currentTimeMillis())
    MongoDBObject(fields ++ user.map(x => ("user" -> x)).toList)
  }

}
case class GetUrlFor(bookmark: Bookmark)
case object EmptyUrl
case object InitSpace
case object IsInitFinished
case class MiniUrl(postfix: String)
case class GenerateUrls(count: Integer)
case class UrlList(list: List[String])

class UrlSpace(minPool: Int, maxPool: Int, db: UrlMongoPersistence, urlLength: Integer = 6) extends Actor {
  implicit val timeout = akka.util.Timeout(5 second)
  val urlGenerator = context.system.actorOf(Props(new UrlGenerator(db)))
  var initFinished = false
  var urls = new ListBuffer[String]()

  override def receive = {
    case GetUrlFor(b: Bookmark) =>
      urls.size match {
        case 0 => sender() ! EmptyUrl
        case `minPool` =>
          urlGenerator ! GenerateUrls(maxPool - minPool)
          val last = urls.remove(urls.size - 1)
          db.TakeMiniUrlById(last, b)
          sender() ! MiniUrl(last)
        case _ =>
          val last = urls.remove(urls.size - 1)
          db.TakeMiniUrlById(last, b)
          sender() ! MiniUrl(last)
      }
    case InitSpace =>
      urlGenerator ! GenerateUrls(maxPool)
    case IsInitFinished =>
      sender() ! (initFinished)
    case UrlList(list) =>
      urls ++= list
      initFinished = true
    case _ => println("huh?")
  }
}

class UrlGenerator(db: UrlMongoPersistence) extends Actor {
  //  val domain =Play.current.configuration.getString("server.domain")
  val gen = MiniUrlGen(db)
  override def receive = {
    case GenerateUrls(c: Integer) =>
      val res = (1 to c).map(x => gen.genUrl()).toList
      sender() ! UrlList(res)
    case _ => println("huh?")
  }
}

