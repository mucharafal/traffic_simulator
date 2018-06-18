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
    "return correct information about itself 1 road" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(rightHandJunction))

      junction ! AddRoad(mockRoad.ref, Map("begin" -> true))
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple2(rightHandJunction, List(Tuple2(mockRoad.ref, true)))))
    }
    "return correct information about itself 10 roads (adding is the same in every junction, so it is not copied)" in {
      val beginRoads = (1 to 10).map(x => TestProbe())
      val endRoads = (1 to 5).map(_ => TestProbe())
      val junction = system.actorOf(Junction.props(rightHandJunction))
      val mock = TestProbe()

      for(road <- beginRoads) {
        junction ! AddRoad(road.ref, Map("begin" -> true))
      }
      for(road <- endRoads){
        junction ! AddRoad(road.ref, Map("begin" -> false))
      }

      val roads = (beginRoads.map(x => (x.ref, true)).toList ++ endRoads.map(x => (x.ref, false)).toList)

      mock.send(junction, JunctionGetInformationRequest)
      mock.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple2(rightHandJunction, roads)))
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
    "correct set priviledge roads" in {
      import SignJunction._
      val road1 = TestProbe()
      val road2 = TestProbe()
      val junction = system.actorOf(Junction.props(signJunction))

      val mockRoad = TestProbe()

      junction ! AddRoad(road1.ref, Map("begin" -> true))
      junction ! AddRoad(road2.ref, Map("begin" -> true))

      val beginRoads = List(road1, road2)

      val roads = beginRoads.map(x => (x.ref, true)).toList

      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple3(signJunction, roads, (null, null))))

      junction ! PriviledgeRoad(road2.ref)

      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple3(signJunction, roads, (road2.ref, null))))

      junction ! PriviledgeRoad(road1.ref)

      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple3(signJunction, roads, (road2.ref, road1.ref))))

      junction ! PriviledgeRoad(road2.ref)

      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple3(signJunction, roads, (road2.ref, road1.ref))))

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
    "return correct value of time to light change" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signalizationJunction))

      junction ! AddRoad(mockRoad.ref, Map("begin" -> true))
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, true)),
            null, 10)))
      import TimeSynchronizer._

      junction ! ComputeTimeSlot(0)
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(0,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, true)),
            null, 9)))
    }
    "change the light" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signalizationJunction))

      junction ! AddRoad(mockRoad.ref, Map("begin" -> false))
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, false)),
            null, 10)))
      import TimeSynchronizer._

      for(i <- 1 to 10)
        junction ! ComputeTimeSlot(i)

      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(10,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, false)),
            null, 0)))

      junction ! ComputeTimeSlot(0)
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(0,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, false)),
            mockRoad.ref , 10)))
    }
    "n't change the light when there is no road to junction" in {
      val mockRoad = TestProbe()
      val junction = system.actorOf(Junction.props(signalizationJunction))

      junction ! AddRoad(mockRoad.ref, Map("begin" -> true))
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(-1,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, true)),
            null, 10)))
      import TimeSynchronizer._

      for(i <- 1 to 10)
        junction ! ComputeTimeSlot(i)

      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(10,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, true)),
            null, 0)))

      junction ! ComputeTimeSlot(0)
      mockRoad.send(junction, JunctionGetInformationRequest)
      mockRoad.expectMsg(100 millis,
        JunctionGetInformationResult(0,
          Tuple4(signalizationJunction, List(Tuple2(mockRoad.ref, true)),
            null, 10)))
    }
  }
}