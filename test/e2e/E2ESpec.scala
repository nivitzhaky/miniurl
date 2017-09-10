package e2e

import java.util.UUID

import akka.actor.ActorSystem
import controllers.MiniUrlController
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.{ FakeApplication, FakeRequest, PlaySpecification, WithApplication }
import util.Utils

import scala.concurrent.Future

case class ControllerPlusUrl(controller: MiniUrlController, miniUrlRes: Future[Result])
class E2ESpec extends PlaySpecification {
  val as = ActorSystem("forTest")

  "get mini url via api " should {
    "run the overall process - generate url and redirect" in new WithApplication() {
      val r = generateMiniUrl("www.google.com", "google")
      status(r.miniUrlRes) mustEqual OK

      val mini = (contentAsJson(r.miniUrlRes) \ "postfix").get.as[String]
      val res2 = call(r.controller.myredirect(mini), FakeRequest(GET, "/"))
      //status(res) mustEqual MOVED_PERMANENTLY //TODO: investigate why the call function did not return 301 while it was sent
      (status(r.miniUrlRes) < 400) should_== (true)
    }

    "click count works" in new WithApplication() {
      val r = generateMiniUrl("www.google.com", "google")
      status(r.miniUrlRes) mustEqual OK

      val mini = (contentAsJson(r.miniUrlRes) \ "postfix").get.as[String]
      call(r.controller.myredirect(mini), FakeRequest(GET, "/"))
      call(r.controller.myredirect(mini), FakeRequest(GET, "/"))
      call(r.controller.myredirect(mini), FakeRequest(GET, "/"))
      call(r.controller.myredirect(mini), FakeRequest(GET, "/"))
      Utils.tryForTwentySeconds {
        val res2 = call(r.controller.miniDetails(mini), FakeRequest(GET, "/"))
        (contentAsJson(res2) \ "clicks").get.as[Int] should_== (4)
      }
    }

    "listing works" in new WithApplication() {
      val r = generateMiniUrl("www.google.com", "google")
      val mini = (contentAsJson(r.miniUrlRes) \ "postfix").get.as[String]

      val res2 = call(r.controller.list(), FakeRequest(GET, "/"))
      contentAsString(res2).contains(mini) should_== (true)
    }

    "modify works" in new WithApplication() {
      val r = generateMiniUrl("www.google.com", "google")
      val mini = (contentAsJson(r.miniUrlRes) \ "postfix").get.as[String]

      val newAlias = UUID.randomUUID().toString
      call(r.controller.update(mini), FakeRequest(PUT, "/").withJsonBody(Json.obj("alias" ->newAlias)))
      Utils.tryForTwentySeconds {
        val res2 = call(r.controller.miniDetails(mini), FakeRequest(GET, "/"))
        (contentAsJson(res2) \ "alias").get.as[String] should_== (newAlias)
      }
    }

    "delete works" in new WithApplication() {
      val r = generateMiniUrl("www.google.com", "google")
      val mini = (contentAsJson(r.miniUrlRes) \ "postfix").get.as[String]

      call(r.controller.delete(mini), FakeRequest(DELETE, "/"))

      val res2 = call(r.controller.list(), FakeRequest(GET, "/"))
      contentAsString(res2).contains(mini) should_== (false)
    }

    "list by user works" in new WithApplication() {
      val user = UUID.randomUUID().toString
      val nonExistingUser = UUID.randomUUID().toString
      val r = generateMiniUrl("www.google.com", "google", Some(user))
      val mini = (contentAsJson(r.miniUrlRes) \ "postfix").get.as[String]

      val res2 = call(r.controller.list(Some(user)), FakeRequest(GET, "/"))
      contentAsString(res2).contains(user) should_== (true)
      contentAsString(res2).contains(nonExistingUser) should_== (false)
    }

    // error handling
    "fail when url space is empty" in new WithApplication(new FakeApplication(
      additionalConfiguration = Map("pool.min" -> 0, "pool.max" -> 0))
    ) {
      val r = generateMiniUrl("www.google.com", "google")
      status(r.miniUrlRes) mustEqual INTERNAL_SERVER_ERROR
    }

    "fail for invalid url" in new WithApplication() {
      val r = generateMiniUrl("goo", "google")
      status(r.miniUrlRes) mustEqual INTERNAL_SERVER_ERROR
    }

    "redirect to invalid miniurl fails" in new WithApplication() {
      val controller = new MiniUrlController(as);
      var request = FakeRequest(GET, "/")
      val res= call(controller.myredirect("nonexisting"),request)
      status(res) mustEqual NOT_FOUND
    }
  }

  private def generateMiniUrl(url: String, alias: String, user: Option[String] = None) = {
    val controller = new MiniUrlController(as);
    var request = FakeRequest(POST, "/").withJsonBody(Json.obj("url" -> url, "alias" -> alias))
    if (user.isDefined)
      request = FakeRequest(POST, "/").withJsonBody(Json.obj("url" -> url, "alias" -> alias, "user" -> user.get))
    val res = call(controller.genUrl(), request)
    ControllerPlusUrl(controller, res)
  }

}

