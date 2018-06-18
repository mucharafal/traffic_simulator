package com.simulator

import org.scalatest.{ BeforeAndAfterAll, WordSpecLike, Matchers }
import akka.actor.ActorSystem
import akka.testkit.{ TestKit, TestProbe }
import scala.concurrent.duration._
import scala.language.postfixOps

import Junction._
import JunctionTypes._


class JunctionTest(_system: ActorSystem) extends TestKit(_system)
  with Matchers with WordSpecLike with BeforeAndAfterAll {

  def this() = this(ActorSystem("JunctionTest"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  "A right hand junction" should {
    "return correct information about itself 1" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(rightHandJunction))

      junction ! AddRoad(mockRoad.ref, Map("begin" -> true))
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple2(rightHandJunction, List(Tuple2(mockRoad.ref, true)))))
    }
  }
  "A sign junction" should {
    "return correct information about itself 1" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signJunction))

      junction ! AddRoad(mockRoad.ref, Map("begin" -> true))
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple3(signJunction, List(Tuple2(mockRoad.ref, true)), (null, null))))
    }
  }
  "A signalization junction" should {
    "return correct information about itself 1" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signalizationJunction))

      junction ! AddRoad(mockRoad.ref, Map("begin" -> true))
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, true)),
            null, 10)))
    }
  }
}