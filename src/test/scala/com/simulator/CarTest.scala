package com.simulator

import org.scalatest.{ BeforeAndAfterAll, WordSpecLike, Matchers }
import akka.actor.ActorSystem
import akka.testkit.{ TestKit, TestProbe }
import scala.concurrent.duration._
import scala.language.postfixOps

import Junction._
import JunctionTypes._
import SignJunction._

import TimeSynchronizer._
import Road._
import Car._

class CarTest(_system: ActorSystem) extends TestKit(_system)
  with Matchers with WordSpecLike with BeforeAndAfterAll {

  def this() = this(ActorSystem("JunctionTest"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  "A car" should {
    "return correct information about itself" in {
      val road = TestProbe()
      val car = system.actorOf(Car.props((road.ref, 0), (road.ref, 1), 0))

      road.send(car, CarGetInformationRequest)
      road.expectMsg(100 millis, CarGetInformationResult(
        road.ref, 0, 0, false
      ))
    }

  }

}