package com.simulator.roadgeneration

import com.simulator.common.Snapshot

trait RoadGenerationService {
  def generate(junctionCount: Int, carCount: Int): Snapshot
}
