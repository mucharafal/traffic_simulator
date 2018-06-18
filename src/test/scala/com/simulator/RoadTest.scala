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

  // TODO
//  "A Road" should {
//    "return null, when has no car on" in {
//      val mock = TestProbe()
//      val road = system.actorOf(Road.props(mock.ref, mock.ref, mock.ref))
//
//      mock.send(road, GetCarIdNthCar(1))
//      mock.expectMsg(500 millis, NoCar)
//      mock.send(road, GetCarIdNthCar(2))
//      mock.expectMsg(500 millis, NoCar)
//      mock.send(road, GetCarIdNthCar(100))
//      mock.expectMsg(500 millis, NoCar)
//    }
//    "return proper ref, when it is car on" in {
//      val mock = TestProbe()
//      val road = system.actorOf(Road.props(mock.ref, mock.ref, mock.ref))
//
//      mock.send(road, AddCar(mock.ref, 1.0))
//      mock.send(road, GetCarIdNthCar(1))
//      mock.expectMsg(500 millis, CarRef(mock.ref))
//
//      mock.send(road, GetCarIdNthCar(2))
//      mock.expectMsg(500 millis, NoCar)
//    }
//  }

}