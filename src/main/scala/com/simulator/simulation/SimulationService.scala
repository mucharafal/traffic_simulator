package com.simulator.simulation

import com.simulator.common.Snapshot

trait SimulationService {
  def simulateTimeSlot(): Snapshot
}
