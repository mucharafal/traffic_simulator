package com.simulator.roadgenerator

import com.simulator.ui._
import io.github.jdiemke.triangulation.{DelaunayTriangulator, Vector2D}

import scala.collection.JavaConverters._
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.util.Random

class RoadGenerator {
  private val r = new Random()

  def generate(junctionCount: Int, carCount: Int): Snapshot = {
    val junctions: Seq[Junction] = generateJunctions(junctionCount)
    val roads: Seq[Road] = generateRoads(junctions)
    val cars: Seq[Car] = generateCars(roads.toIndexedSeq, carCount)

    Snapshot(cars, roads, junctions)
  }

  private def generateJunctions(junctionCount: Int): Seq[Junction] = {
    for (id <- 0 to junctionCount) yield Junction(id, Position(r.nextInt(1000), r.nextInt(500)))
  }

  private def generateRoads(junctions: Seq[Junction]): Seq[Road] = {
    val vectorToJunctionMap: Map[Vector2D, Junction] = junctions
      .map { junction => positionToVector2d(junction.position) -> junction }
      .toMap

    val triangulator = new DelaunayTriangulator(vectorToJunctionMap.keys.toSeq.asJava)
    triangulator.triangulate()

    triangulator.getTriangles.asScala.to[Seq]
      .flatMap { triangle => Seq(triangle.a, triangle.b, triangle.c).combinations(2) }
      .map { _.toSet }
      .distinct
      .map { _.toSeq.map(vectorToJunctionMap) }
      .zipWithIndex
      .map { case (Seq(start, end), id) => Road(id, start, end) }
  }

  private def generateCars(roads: IndexedSeq[Road], carCount: Int): Seq[Car] = {
    def interpolateRoad(road: Road, alpha: Float) =
      road.start.position.interpolate(road.end.position)(alpha)

    Seq.fill(carCount) { roads(r.nextInt(roads.size)) }
      .map { interpolateRoad(_, r.nextFloat()) }
      .zipWithIndex
      .map { case (position, id) => Car(id, position) }
  }

  private def positionToVector2d(p: Position): Vector2D = new Vector2D(p.x, p.y)
  private def vector2dToPosition(v: Vector2D): Position = Position(v.x.toFloat, v.y.toFloat)

}
