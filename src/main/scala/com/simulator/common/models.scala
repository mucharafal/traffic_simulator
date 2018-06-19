package com.simulator.common

import scala.collection.immutable.Seq

case class Junction(id: JunctionId,
                    position: Position)

case class Road(id: RoadId,
                start: JunctionId,
                end: JunctionId)

case class Car(id: CarId,
               road: RoadId,
               positionOnRoad: Float,
               velocity: Float = 0.0f,
               breaking: Boolean = false)

case class Snapshot(junctions: Seq[Junction],
                    roads: Seq[Road],
                    cars: Seq[Car])
