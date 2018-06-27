package com.simulator.common

import scala.collection.generic.MapFactory
import scala.collection.immutable
import scala.language.implicitConversions

class BiMap[K, V](private val map: immutable.Map[K, V])
  extends immutable.Map[K, V]
    with immutable.MapLike[K, V, BiMap[K, V]] {

  @transient
  lazy val inverse: BiMap[V, K] = new BiMap(map map { _.swap })

  override def +[V1 >: V](kv: (K, V1)): BiMap[K, V1] = new BiMap(map + kv)
  override def get(key: K): Option[V] = map.get(key)
  override def iterator: Iterator[(K, V)] = map.iterator
  override def -(key: K): BiMap[K, V] = new BiMap(map - key)
  override def empty: BiMap[K, V] = BiMap.empty
}

object BiMap extends MapFactory[BiMap] {
  override def empty[A, B]: BiMap[A, B] = new BiMap(immutable.Map.empty)

  implicit def map2BiMap[K, V](map: immutable.Map[K, V]): BiMap[K, V] = new BiMap(map)
}