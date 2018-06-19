package com.simulator

import org.scalatest.{ BeforeAndAfterAll, WordSpecLike, Matchers }
import akka.actor.ActorSystem
import akka.testkit.{ TestKit, TestProbe }
import scala.concurrent.duration._
import scala.language.postfixOps
import Road._

class RoadTest(_system: ActorSystem) extends TestKit(_system)
  with Matchers with WordSpecLike with BeforeAndAfterAll{

  def this() = this(ActorSystem("RoadTest"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  "A Road" should {
    "return null, when has no car on" in {
      val mock = TestProbe()
      val road = system.actorOf(Road.props(mock.ref, mock.ref, 5.0))

      mock.send(road, GetNthCar(1))
      mock.expectMsg(500 millis, NthCar(None))
      mock.send(road, GetNthCar(2))
      mock.expectMsg(500 millis, NthCar(None))
      mock.send(road, GetNthCar(100))
      mock.expectMsg(500 millis, NthCar(None))
    }
    "return proper ref, when it is car on" in {
      val mock = TestProbe()
      val road = system.actorOf(Road.props(mock.ref, mock.ref, 5.0))

      mock.send(road, AddCar(mock.ref, 1.0))
      mock.send(road, GetNthCar(1))
      mock.expectMsg(500 millis, NthCar(Some(mock.ref)))

      mock.send(road, GetNthCar(2))
      mock.expectMsg(500 millis, NthCar(None))
    }
    "correct add and remove cars" in {
      val cars = (1 to 10).map(_ => TestProbe())
      val road = system.actorOf(Road.props(mock.ref, mock.ref, 5.0))

      cars.foreach(r => road ! AddCar(r.ref))
      //todo
    }
  }

}
