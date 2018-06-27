package com.simulator.common

object CollectionExtensions {

  implicit class TraversableKeyBy[T](val traversable: Traversable[T]) extends AnyVal {
    def keyBy[K](f: T => K): Map[K, T] =
      traversable.map { elem => f(elem) -> elem }.toMap
  }

}

