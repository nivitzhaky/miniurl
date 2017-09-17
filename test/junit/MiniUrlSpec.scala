package junit
import java.util.UUID

import akka.actor.ActorSystem
import akka.pattern.ask
import org.json4s.DefaultFormats
import org.specs2.matcher.{ MatcherMacros, MustMatchers, ResultMatchers }
import org.specs2.mutable.Specification
import persistence.MiniUrlGen
import space._

import scala.concurrent.duration._

class MiniUrlSpec extends Specification with ResultMatchers with MatcherMacros with MustMatchers {

  import util.Utils._
  implicit val formats = new DefaultFormats {}
  implicit val as = ActorSystem("ForTest")
  implicit val timeout = akka.util.Timeout(5 second)

  "urlPool" should {
    "Empty Space is empty " in new UrlTestContext {
      val space = givenEmptyUrlSpace
      waitFor(space ? GetUrlFor(book1)) should_== (EmptyUrl)
    }
    "Empty Space init works " in new UrlTestContext {
      val space = givenTenUrlSpace
      space ! InitSpace
      tryForTwentySeconds(waitFor(space ? IsInitFinished) should_== (true))
    }

    "Non empty Space is returning data" in new UrlTestContext {
      val space = givenInitializedUrlSpace(as, 10)
      (1 to 10).foreach(x => waitForMini(space ? GetUrlFor(book1)))
    }
    "We get the mini urls by order of insertion" in new UrlTestContext {
      val space = givenInitializedUrlSpace(as, 10)
      val inserted = (1 to 10).map(x => book1.copy(user = Some(userid))).map(b => waitForMini(space ? GetUrlFor(b)))
        .map(u => u.postfix).reverse.toList
      val fromDB = db.getUsedUrls(Some(userid)).take(10).map(u => u._id)
      inserted should_== (fromDB)
    }

    "taken miniurl returns the same data" in new UrlTestContext {
      val space = givenInitializedUrlSpace(as, 10)
      val mini = waitForMini(space ? GetUrlFor(book1))
      db.getUrlByMini(mini.postfix) should_== (book1)
    }

    "taken miniurl with user defines returns the same data" in new UrlTestContext {
      val space = givenInitializedUrlSpace(as, 10)
      val mini = waitForMini(space ? GetUrlFor(book1.copy(user = Some("user"))))
      db.getUrlByMini(mini.postfix) should_== (book1.copy(user = Some("user")))
    }

    "List by user returns data" in new UrlTestContext {
      val space = givenInitializedUrlSpace(as, 10)
      val user = UUID.randomUUID().toString
      val mini = waitForMini(space ? GetUrlFor(book1.copy(user = Some(user))))
      db.getUsedUrls(Some(user)).toString().contains(mini.postfix) should_== (true)
    }

    "Space is extending" in new UrlTestContext {
      val space = givenInitializedUrlSpace(as, 10)
      (1 to 10).foreach(x => waitForMini(space ? GetUrlFor(book1)))
      tryForTwentySeconds(
        (1 to 20).foreach(x => waitForMini(space ? GetUrlFor(book1)))
      )
    }

    "generate Url " in new UrlTestContext {
      urlGen.genUrl() must not be ("")
    }

    "generate too many short url will throw exception" in new UrlTestContext {
      ({
        val smallSpacGen = MiniUrlGen(db, length = 1)
        (1 to 1000).foreach(x => smallSpacGen.genUrl())
      } must throwAn[RuntimeException]).message.contains("Unable to create a key") should_== (true)
    }

    "generate standard url will create a unique list" in new UrlTestContext {
      (1 to 100).map(x => urlGen.genUrl()).toSet.size must_== (100)

    }
  }

}

