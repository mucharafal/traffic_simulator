package com.simulator.roadgeneration

import com.simulator.common.Snapshot

trait RoadGenerationService {
  def generate(wordSize: Int, junctionCount: Int, carCount: Int): Snapshot
}
