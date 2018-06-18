package com.simulator

import org.scalatest.{ BeforeAndAfterAll, WordSpecLike, Matchers }
import akka.actor.ActorSystem
import akka.testkit.{ TestKit, TestProbe }
import scala.concurrent.duration._
import scala.language.postfixOps
import TimeSynchronizer._

class TimeSynchronizerTest(_system: ActorSystem)
  extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("TimeSynchronizerTest"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  "A Time Synchronizer" should {
    "send signal, when has one registered client" in {
      val clientProbe = TestProbe()
      val timeSynchronizer = system.actorOf(TimeSynchronizer.props())

      timeSynchronizer ! AddObject(clientProbe.ref)
      clientProbe.send(timeSynchronizer, Computed)
      clientProbe.expectMsg(500 millis, ComputeTimeSlot(0))
    }
    "send signals, when receive complete signal from client (1 client version)" in {
      val clientProbe = TestProbe()
      val timeSynchronizer = system.actorOf(TimeSynchronizer.props())

      timeSynchronizer ! AddObject(clientProbe.ref)

      for(i <- 1 to 10){
        clientProbe.send(timeSynchronizer, Computed)
        clientProbe.expectMsg(500 millis, ComputeTimeSlot(i-1))
      }
    }
    "send signals, when receive complete signal from all clients (client registered 10 times)" in {
      val clientProbe = TestProbe()
      val timeSynchronizer = system.actorOf(TimeSynchronizer.props())

      for(i <- 1 to 10) {
        timeSynchronizer ! AddObject(clientProbe.ref)
      }

      for(i1 <- 1 to 10){
        for(i <- 1 to 10) {
          clientProbe.send(timeSynchronizer, Computed)
        }

        for(i <- 1 to 10) {
          clientProbe.expectMsg(500 millis, ComputeTimeSlot(i1-1))
        }
      }
    }
    "send signals, when receive complete signal from all clients (number of clients changed)" in {
      val clientsProbe = (1 to 10).map(x => TestProbe())
      val timeSynchronizer = system.actorOf(TimeSynchronizer.props())

      for(client <- clientsProbe){
        timeSynchronizer ! AddObject(client.ref)
      }

      for(i <- 1 to 10) {
        for (client <- clientsProbe) {
          client.send(timeSynchronizer, Computed)
        }
        for(client <- clientsProbe){
          client.expectMsg(500 millis, ComputeTimeSlot(i-1))
        }
      }

      val ignoredIndex = 2
      timeSynchronizer ! RemoveObject(clientsProbe(ignoredIndex).ref)

      for(i <- 1 to 10) {
        for (client <- clientsProbe) {
          for(client1 <- clientsProbe){
            client1.expectNoMessage(10 millis)
          }
          client.send(timeSynchronizer, Computed)
        }
        for(client <- clientsProbe){
          if(client == clientsProbe(ignoredIndex)){
            client.expectNoMessage(10 millis)
          } else {
            client.expectMsg(10 millis, ComputeTimeSlot(i+9))
          }
        }
      }
    }
  }
}