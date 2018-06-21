package com.simulator.roadgeneration

import com.simulator.common._
import io.github.jdiemke.triangulation.{DelaunayTriangulator, Vector2D}

import scala.collection.JavaConverters._
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.util.Random

class RoadGenerationServiceImpl extends RoadGenerationService {
  private val r = new Random()

  override def generate(junctionCount: Int, carCount: Int): Snapshot = {
    val junctions: Seq[JunctionState] = generateJunctions(junctionCount)
    val roads: Seq[RoadState] = generateRoads(junctions)
    val cars: Seq[CarState] = generateCars(roads.toIndexedSeq, carCount)

    Snapshot(junctions, roads, cars)
  }

  private def generateJunctions(junctionCount: Int): Seq[JunctionState] = {
    for (id <- 0 to junctionCount) yield JunctionState(JunctionId(id), Position(r.nextInt(1000), r.nextInt(500)))
  }

  private def generateRoads(junctions: Seq[JunctionState]): Seq[RoadState] = {
    val vectorToJunctionMap: Map[Vector2D, JunctionState] = junctions
      .map { junction => positionToVector2d(junction.position) -> junction }
      .toMap

    val triangulator = new DelaunayTriangulator(vectorToJunctionMap.keys.toSeq.asJava)
    triangulator.triangulate()

    triangulator.getTriangles.asScala.to[Seq]
      .flatMap { triangle => Seq(triangle.a, triangle.b, triangle.c).combinations(2) }
      .map { _.toSet }
      .distinct
      .map { _.toSeq.map(vectorToJunctionMap) }
      .flatMap { case Seq(start, end) =>
        Seq((start, end), (end, start))
      }
      .zipWithIndex
      .map { case ((start, end), id) =>
        RoadState(RoadId(id), start.id, end.id),
      }
  }

  private def generateCars(roads: IndexedSeq[RoadState], carCount: Int): Seq[CarState] = {
    Seq.fill(carCount) { roads(r.nextInt(roads.size)) }
      .zipWithIndex
      .map { case (road, id) => CarState(CarId(id), road.id, r.nextFloat()) }
  }

  private def positionToVector2d(p: Position): Vector2D = new Vector2D(p.x, p.y)
  private def vector2dToPosition(v: Vector2D): Position = Position(v.x.toFloat, v.y.toFloat)

}
