package com.simulator

import akka.actor.{Props}

object KeyGenerator {
  def props(): Props = Props(new KeyGenerator(0))
  final case class GetKey(From: Int)
  final case class ReceivedKey(Key: Int)
  case object ComputeTimeSlot
}

class KeyGenerator(var key: Int) {
  def receive() = {
    case GetKey(from) => {
      from ! ReceivedKey(key)
      key = key + 1
    }
  }
}

