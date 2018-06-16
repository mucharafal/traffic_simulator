package com.simulator

import javax.sql.RowSetMetaData

object Junction {
  def props(): Props = Props(Junction)
  final case class JunctionGetInformationRequest(From: Int)
  final case class JunctionGetInformationResult(From: Int, informationPackage)
}

class Junction(val JunctionId: Int, var RoadsList: List[Road]) {

  def addRoad(newRoad: Road): Unit = {
    RoadsList = RoadsList ++ List(newRoad)
  }


}