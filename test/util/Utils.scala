package util

import org.scalatest.concurrent.Eventually
import space.{ GetUrlFor, MiniUrl }

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.{ Duration, MILLISECONDS, SECONDS }

object Utils extends Eventually {
  def tryForTwentySeconds(code: => Unit): Unit = {
    eventually(timeout(Duration(20, SECONDS)), interval(Duration(200, MILLISECONDS))) {
      code
    }
  }
  def tryForOneMinute(code: => Unit): Unit = {
    eventually(timeout(Duration(60, SECONDS)), interval(Duration(1, SECONDS))) {
      code
    }
  }

  def waitFor(f: Future[Any]) = {
    Await.result(f, Duration.Inf)
  }
  def waitForMini(f: Future[Any]) = {
    Await.result(f, Duration.Inf).asInstanceOf[MiniUrl]
  }

  def ignoreCode(code: => Unit): Unit = {

  }

}
