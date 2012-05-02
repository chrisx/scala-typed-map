package com.bookish.common

/**
 * Map that retains type information of heterogeneous element keys and values
 * (Manifests only allow derivation of erased types from homogeneous collection types.
 * A collection with multiple types are generalised to the highest common type, so
 * List("a",1) is a List[Any])
 * This map is backed by a mutable Map for now.
 */
class MMap {
  private var _map = scala.collection.mutable.Map.empty[Any, (Manifest[_], Any)]

  def update[T: Manifest](name: Any, item: T) = {
    _map(name) = (manifest[T], item)
    this
  }
  def flatMap[B](f: ((Any, (Manifest[_], Any))) => Traversable[B]): Traversable[B] = _map.flatMap(a => f(a))
  def map[B](f: ((Any, (Manifest[_], Any))) => B): Iterable[B] = _map.map(a => f(a))
  def foreach(f: ((Any, (Manifest[_], Any))) => Unit) = _map.foreach(f)
  def apply[T: Manifest](key: Any): T = get(key).get
  def apply[V: Manifest](kv: (Any, V)): MMap = update(kv._1, kv._2)

  def get[T: Manifest](key: Any): Option[T] = {
    val o = _map.get(key)

    o match {
      case Some((om: Manifest[_], s: Any)) =>
        if (om <:< manifest[T]) Some(s.asInstanceOf[T]) else None
      case _ => None
    }
  }
  def toMap = _map.toMap
  override def toString = "MMAP: { " + _map.toString + " }"
}

object MMap {
  def empty = new MMap()
  // convenience methods to emulate scala.collection.Map
  // need to define apply methods explicitly, to avoid erasure in multiple parameter array
  def apply[V: Manifest](kv: (Any, V)): MMap = {
    val m = new MMap()
    m.update[V](kv._1, kv._2)
  }

  def apply[V1: Manifest, V2: Manifest](kv1: (Any, V1), kv2: (Any, V2)): MMap = {
    val m = MMap(kv1._1 -> kv1._2)
    m.update(kv2._1, kv2._2)
  }

  def apply[V1: Manifest, V2: Manifest, V3: Manifest](kv1: (Any, V1), kv2: (Any, V2), kv3: (Any, V3)): MMap = {
    val m = MMap(kv1._1 -> kv1._2, kv2._1 -> kv2._2)
    m.update(kv3._1, kv3._2)
  }

  def apply[V1: Manifest, V2: Manifest, V3: Manifest, V4: Manifest](kv1: (Any, V1), kv2: (Any, V2), kv3: (Any, V3), kv4: (Any, V4)): MMap = {
    val m = MMap(kv1._1 -> kv1._2, kv2._1 -> kv2._2, kv3._1 -> kv3._2)
    m.update(kv4._1, kv4._2)
  }
}
