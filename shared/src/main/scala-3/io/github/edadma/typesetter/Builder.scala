package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer

trait Builder extends Mode:
  protected val boxes = new ArrayBuffer[Box]

  def init(): Unit = ()

  def clear(): Unit = boxes.clear()

  def beforeLast: Box = boxes(boxes.length - 2)

  def last: Box = boxes.last

  def lastOption: Option[Box] = boxes.lastOption

  def dropRightInPlace(n: Int): Unit = boxes dropRightInPlace n

  def removeLast(): Box = boxes.remove(boxes.length - 1)

  def length: Int = boxes.length

  def update(index: Int, elem: Box): Unit = boxes.update(index, elem)

  def insert(idx: Int, box: Box): Unit = boxes.insert(idx, box)

  def nonEmpty: Boolean = boxes.nonEmpty

  def isEmpty: Boolean = boxes.isEmpty

  infix def add(b: Box): Unit = boxes += b

  infix def addSeq(bs: scala.collection.Seq[Box]): Unit = boxes ++= bs
