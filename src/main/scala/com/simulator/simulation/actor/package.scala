package com.simulator.simulation

import akka.actor.ActorRef

package object actor {

  type JunctionRef = ActorRef
  type RoadRef = ActorRef
  type CarRef = ActorRef
  type TimeSynchronizerRef = ActorRef

}
