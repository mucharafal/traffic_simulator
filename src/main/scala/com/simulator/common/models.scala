package com.simulator.common

import scala.collection.immutable.Seq

case class JunctionState(id: JunctionId,
                         position: Position)

case class RoadState(id: RoadId,
                     start: JunctionId,
                     end: JunctionId)

case class CarState(id: CarId,
                    road: RoadId,
                    positionOnRoad: Float,
                    velocity: Float = 0.0f,
                    breaking: Boolean = false)

case class Snapshot(junctions: Seq[JunctionState],
                    roads: Seq[RoadState],
                    cars: Seq[CarState])
