package controllers

import javax.inject._

import akka.actor.{ ActorRef, ActorSystem, Props }
import com.github.tototoshi.play2.json4s.native.Json4s
import persistence.{ DBName, MongoBookmark, UrlMongoPersistence }
import space._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try

//import play.api.libs.json.Json
import akka.pattern.ask
import org.json4s._
import play.api.Play.current
import play.api.libs.json._
import play.api.mvc._
import utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
case class JustAlias(alias: String)

@Singleton
class MiniUrlController @Inject() (system: ActorSystem) extends Controller with Json4s with Eventually {

  implicit val formats = new DefaultFormats {}
  implicit val as = system
  val domain = current.configuration.getString("server.domain").get
  val poolmax = current.configuration.getInt("pool.max").get
  val poolmin = current.configuration.getInt("pool.min").get
  val db = current.configuration.getString("database.name").get
  val mongoPersistence = new UrlMongoPersistence(db)
  var urlSpace: ActorRef = _
  implicit val timeout = akka.util.Timeout(5 second)

  init()

  def genUrl = Action.async(json) { implicit request =>
    Future {
      Try { request.body.extract[Bookmark] }.flatMap(b =>
        b.validate()
      ).map { book =>
        getMiniUrlFromUrlSpace(book)
      }.map(x => Ok(Json.obj("postfix" -> x.postfix, "link" -> s"$domain/${x.postfix}")).withHeaders(headers: _*))
        .recover {
          case e: Throwable =>
            Results.InternalServerError(Json.obj("error" -> e.getMessage)).withHeaders(headers: _*)
        }.get
    }
  }

  def myredirect(miniurl: String) = Action { implicit request =>
    try {
      val bookmark = mongoPersistence.getUrlByMini(miniurl, true)
      Results.MovedPermanently(bookmark.getUrl()).withHeaders(headers: _*)
    } catch {
      case e: Throwable =>
        Results.NotFound(Json.obj()).withHeaders(headers: _*)
    }
  }

  def list(user: Option[String] = None) = Action { implicit request =>
    val bookmarks = mongoPersistence.getUsedUrls(user).map(x => UiBookmark.fromMongoBookmark(x))
    Ok((Extraction.decompose(bookmarks))).withHeaders(headers: _*)
  }

  def delete(miniurl: String) = Action { implicit request =>
    mongoPersistence.deleteMiniUrl(miniurl)
    Ok(Json.obj()).withHeaders(headers: _*)
  }

  def update(miniurl: String) = Action(json) { implicit request =>
    val a = request.body.extract[JustAlias]
    mongoPersistence.updateAlias(miniurl, a.alias)
    Ok(Json.obj()).withHeaders(headers: _*)

  }

  def miniDetails(miniurl: String) = Action { implicit request =>
    val bookmarks = mongoPersistence.getUrlByMini(miniurl)
    Ok(Extraction.decompose(UiBookmark.fromBookmark(bookmarks, miniurl))).withHeaders(headers: _*)
  }

  private def getMiniUrlFromUrlSpace(book: Bookmark) = {
    val y = waitFor(urlSpace ? GetUrlFor(book))
    if (y == EmptyUrl) {
      throw new RuntimeException("no urls in the pool")
    }
    y.asInstanceOf[MiniUrl]
  }

  def init() = {
    mongoPersistence.createIndexes()
    urlSpace = as.actorOf(Props(new UrlSpace(poolmin, poolmax, mongoPersistence)))
    urlSpace ! InitSpace
    eventually(waitFor(urlSpace ? IsInitFinished) == true, "unable to initialize pool")(times = 30, interval = 200)
  }

  def waitFor(f: Future[Any]) = {
    Await.result(f, Duration.Inf)
  }

  case class UiBookmark(id: String, url: String, alias: String, clicks: Int, link: String, user: Option[String] = None)
  object UiBookmark {
    def fromMongoBookmark(m: MongoBookmark) = UiBookmark(m._id, m.url, m.alias, m.clicks, domain + "/" + m._id, m.user)
    def fromBookmark(b: Bookmark, id: String) = UiBookmark(id, b.url, b.alias, b.clicks, domain + "/" + id, b.user)
  }

  def headers = List(
    "Access-Control-Allow-Origin" -> "*",
    "Access-Control-Allow-Methods" -> "GET, POST, OPTIONS, DELETE, PUT",
    "Access-Control-Max-Age" -> "3600",
    "Access-Control-Allow-Headers" -> "Origin, Content-Type, Accept, Authorization",
    "Access-Control-Allow-Credentials" -> "true"
  )
  def options(p: String) = Action { request =>
    NoContent.withHeaders(headers: _*)
  }

}
