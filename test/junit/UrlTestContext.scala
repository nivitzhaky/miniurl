package junit

import java.util.UUID

import akka.actor.{ ActorSystem, Props }
import akka.pattern.ask
import org.specs2.matcher.{ MustMatchers, MustThrownExpectations }
import org.specs2.specification.Scope
import persistence.{ MiniUrlGen, UrlMongoPersistence }
import space.{ Bookmark, InitSpace, IsInitFinished, UrlSpace }
import util.Utils
import util.Utils.waitFor

import scala.concurrent.duration._

trait UrlTestContext extends Scope with MustThrownExpectations {
  implicit val timeout = akka.util.Timeout(5 second)

  val domain = "http://localhost:9000"

  val book1 = Bookmark("www.google.com", "google")
  val db = new UrlMongoPersistence("test_mini_url")
  db.db.collectionNames()
  val userid = UUID.randomUUID().toString

  val urlGen = MiniUrlGen(db)

  def givenEmptyUrlSpace(implicit as: ActorSystem) = {
    as.actorOf(Props(new UrlSpace(0, 0, db)))
  }
  def givenTenUrlSpace(implicit as: ActorSystem) = {
    as.actorOf(Props(new UrlSpace(5, 10, db)))
  }
  def givenInitializedUrlSpace(implicit as: ActorSystem, size: Integer) = {
    val space = as.actorOf(Props(new UrlSpace(size / 2, size, db)))
    space ! InitSpace
    Utils.tryForTwentySeconds { waitFor(space ? IsInitFinished) must_== (true) }
    space
  }

}
