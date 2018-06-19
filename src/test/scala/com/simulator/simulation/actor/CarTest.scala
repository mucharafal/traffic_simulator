package com.simulator.simulation.actor

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.simulator.common.CarId
import com.simulator.simulation.actor.Car._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._
import scala.language.postfixOps

class CarTest(_system: ActorSystem) extends TestKit(_system)
  with Matchers with WordSpecLike with BeforeAndAfterAll {

  def this() = this(ActorSystem("JunctionTest"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  "A car" should {
    "return correct information about itself" in {
      val road = TestProbe()
      val car = system.actorOf(Car.props(CarId(0), (road.ref, 0), (road.ref, 1), 0))

      road.send(car, GetStatus)
      road.expectMsg(100 millis, GetStatusResult(
        road.ref, 0, 0, false
      ))
    }

  }

}