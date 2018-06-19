package com.simulator.visualization

import com.simulator.common.Snapshot

trait VisualizationService {
  def visualize(data: Snapshot): Unit
}
