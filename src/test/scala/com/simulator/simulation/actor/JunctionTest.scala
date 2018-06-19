package com.simulator.simulation.actor

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import com.simulator.simulation.actor.Junction._
import com.simulator.simulation.actor.JunctionTypes._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._
import scala.language.postfixOps

class JunctionTest(_system: ActorSystem) extends TestKit(_system)
  with Matchers with WordSpecLike with BeforeAndAfterAll {

  def this() = this(ActorSystem("JunctionTest"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  "A right hand junction" should {
    "return correct information about itself 1 road" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(rightHandJunction))

      junction ! AddRoad(mockRoad.ref, OutDirection)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, rightHandJunction, List.empty, List(mockRoad.ref)))
    }
    "return correct information about itself 10 roads (adding is the same in every junction, so it is not copied)" in {
      val outRoads = (1 to 10).map(_ => TestProbe())
      val inRoads = (1 to 5).map(_ => TestProbe())
      val junction = system.actorOf(Junction.props(rightHandJunction))
      val mock = TestProbe()

      for(road <- outRoads) {
        junction ! AddRoad(road.ref, OutDirection)
      }
      for(road <- inRoads){
        junction ! AddRoad(road.ref, InDirection)
      }

      val inRoadRefs = inRoads.map(_.ref).toList
      val outRoadRefs = outRoads.map(_.ref).toList

      mock.send(junction, GetStatus)
      mock.expectMsg(100 millis,
        GetStatusResult(-1, rightHandJunction, inRoadRefs, outRoadRefs))
    }
  }
  "A sign junction" should {
    "return correct information about itself 1" in {

      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signJunction))

      junction ! AddRoad(mockRoad.ref, OutDirection)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signJunction, List.empty, List(mockRoad.ref), (null, null)))
    }
    "correct set priviledge roads" in {
      import SignJunction._
      val road1 = TestProbe()
      val road2 = TestProbe()
      val junction = system.actorOf(Junction.props(signJunction))

      val mockRoad = TestProbe()

      junction ! AddRoad(road1.ref, OutDirection)
      junction ! AddRoad(road2.ref, OutDirection)

      val outRoadRefs = List(road1, road2).map(_.ref)
      val inRoadRefs = List.empty[ActorRef]

      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signJunction, inRoadRefs, outRoadRefs, (null, null)))

      junction ! PriviledgeRoad(road2.ref)

      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signJunction, inRoadRefs, outRoadRefs, (road2.ref, null)))

      junction ! PriviledgeRoad(road1.ref)

      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signJunction, inRoadRefs, outRoadRefs, (road2.ref, road1.ref)))

      junction ! PriviledgeRoad(road2.ref)

      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signJunction, inRoadRefs, outRoadRefs, (road2.ref, road1.ref)))

    }
  }
  "A signalization junction" should {
    "return correct information about itself 1" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signalizationJunction))

      junction ! AddRoad(mockRoad.ref, OutDirection)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signalizationJunction, List.empty, List(mockRoad.ref), (null, 10)))
    }
    "return correct value of time to light change" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signalizationJunction))

      junction ! AddRoad(mockRoad.ref, OutDirection)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signalizationJunction, List.empty, List(mockRoad.ref), (null, 10)))
      import TimeSynchronizer._

      junction ! ComputeTimeSlot(0)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(0, signalizationJunction, List.empty, List(mockRoad.ref), (null, 9)))
    }
    "change the light" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signalizationJunction))

      junction ! AddRoad(mockRoad.ref, InDirection)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signalizationJunction, List(mockRoad.ref), List.empty, (null, 10)))
      import TimeSynchronizer._

      for(i <- 1 to 10)
        junction ! ComputeTimeSlot(i)

      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(10, signalizationJunction, List(mockRoad.ref), List.empty, (null, 0)))

      junction ! ComputeTimeSlot(0)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(0, signalizationJunction, List(mockRoad.ref), List.empty, (mockRoad.ref, 10)))
    }
    "n't change the light when there is no road to junction" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signalizationJunction))

      junction ! AddRoad(mockRoad.ref, OutDirection)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(-1, signalizationJunction, List.empty, List(mockRoad.ref), (null, 10)))
      import TimeSynchronizer._

      for(i <- 1 to 10)
        junction ! ComputeTimeSlot(i)

      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(10, signalizationJunction, List.empty, List(mockRoad.ref), (null, 0)))

      junction ! ComputeTimeSlot(0)
      mockRoad.send(junction, GetStatus)
      mockRoad.expectMsg(100 millis,
        GetStatusResult(0, signalizationJunction, List.empty, List(mockRoad.ref), (null, 10)))
    }
  }
}