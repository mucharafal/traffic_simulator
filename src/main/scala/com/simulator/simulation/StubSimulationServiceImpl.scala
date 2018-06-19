package com.simulator.simulation

import com.simulator.common.Snapshot

class StubSimulationServiceImpl(private val initialState: Snapshot) extends SimulationService {
  override def simulateTimeSlot(): Snapshot = initialState
}
