package com.simulator.simulation

import com.simulator.common.Snapshot

trait SimulationService {
  def initialize()
  def simulateTimeSlot(): Snapshot
}
