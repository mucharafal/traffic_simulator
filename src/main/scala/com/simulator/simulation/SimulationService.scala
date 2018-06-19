package com.simulator.simulation

import akka.Done
import com.simulator.common.Snapshot

import scala.concurrent.Future

trait SimulationService {
  def initialize(): Future[Done]
  def simulateTimeSlot(): Future[Snapshot]
}
