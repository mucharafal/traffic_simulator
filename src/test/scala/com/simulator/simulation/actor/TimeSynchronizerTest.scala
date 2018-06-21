package com.simulator.simulation.actor

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.simulator.simulation.actor.TimeSynchronizer._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._
import scala.language.postfixOps

class TimeSynchronizerTest(_system: ActorSystem)
  extends TestKit(_system) with Matchers with WordSpecLike with BeforeAndAfterAll {

  def this() = this(ActorSystem("TimeSynchronizerTest"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  "A Time Synchronizer" should {
    "send signal, when has one registered car" in {
      val clientProbe = TestProbe()
      val timeSynchronizer = system.actorOf(TimeSynchronizer.props())

      timeSynchronizer ! AddCar(clientProbe.ref)
      clientProbe.send(timeSynchronizer, Start)
      clientProbe.expectMsg(500 millis, ComputeTimeSlot(0))
    }

    "send signals, when receive complete signal from clients (1 car and 1 infrastructure)" in {
      val clientProbe = TestProbe()
      val timeSynchronizer = system.actorOf(TimeSynchronizer.props())

      timeSynchronizer ! AddCar(clientProbe.ref)
      timeSynchronizer ! AddInfrastructure(clientProbe.ref)
      timeSynchronizer ! Start

      for (i <- 1 to 10) {
        clientProbe.expectMsg(500 millis, ComputeTimeSlot(i - 1))
        clientProbe.send(timeSynchronizer, CarComputed)
        clientProbe.expectMsg(500 millis, ComputeTimeSlot(i - 1))
        clientProbe.send(timeSynchronizer, InfrastructureComputed)
      }
    }
    "work correct with many clients" in {
      val noMsgTime = 40 millis
      val msgTime = 40 millis
      val cars = (1 to 10).map(_ => TestProbe())
      val infr = (1 to 5).map(_ => TestProbe())
      val timeSynchronizer = system.actorOf(TimeSynchronizer.props())

      cars.foreach(r => timeSynchronizer ! AddCar(r.ref))
      infr.foreach(r => timeSynchronizer ! AddInfrastructure(r.ref))

      timeSynchronizer ! Start

      for (i <- 1 to 10) {
        cars.foreach(r => r.expectMsg(msgTime, ComputeTimeSlot(i-1)))
        infr.foreach(r => r.expectNoMessage(noMsgTime))

        cars.foreach(r => r.send(timeSynchronizer, CarComputed))
        infr.foreach(r => r.expectMsg(msgTime, ComputeTimeSlot(i-1)))
        cars.foreach(r => r.expectNoMessage(noMsgTime))
        infr.foreach(r => r.send(timeSynchronizer, InfrastructureComputed))
      }
    }
    //TODO: tests for adding and removing
  }
}