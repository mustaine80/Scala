package com.nframework.nom

class NDouble(s: Double) extends NValueType {
  var value: Double = s

  dataType = EDataType.DOUBLE
  length = 8
  typeLength = 8

  def this() = this(0)

  def toInt(): Int = {
    value.asInstanceOf[Int]
  }

  def toShort(): Short = {
    value.asInstanceOf[Short]
  }

  def toChar(): Char = {
    value.asInstanceOf[Char]
  }

  def toByte(): Byte = {
    value.asInstanceOf[Byte]
  }

  def toFloat(): Float = {
    value.asInstanceOf[Float]
  }

  def toDouble(): Double = {
    value.asInstanceOf[Double]
  }

  override def toString(): String = {
    value.toString()
  }

  def setValue(valueType: NValueType) : Boolean = {
    this.value = valueType.asInstanceOf[NDouble].value

    true
  }

  def getClone() : NValueType = {
    val clone = new NDouble(value)

    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable

    clone
  }

  def copyTo(to: NValueType) {
    to.asInstanceOf[NDouble].value = value
  }

  def serialize() : (Array[Byte], Int) = {
    //val buffer = new scala.collection.mutable.ArrayBuffer[Byte]
    val bb = java.nio.ByteBuffer.allocate(length)

    bb.putDouble(value)

    var buffer = bb.array()

    if(bigEndian) {

    } else {
      // java.nio.ByteBuffer는 Big Endian 인디코딩이 기본 설정이므로 value가 little endian인 경우는 byte 순서를 바꿔줘야 함
      buffer = NValueType.reverseBytes(buffer)
    }

    ( buffer.toArray, length )
  }

  def deserialize(data: Array[Byte], offset: Int) : Int = {
    var v: Double = 0

    val arr = new Array[Byte](length)

    Array.copy(data, offset, arr, 0, length)

    if(bigEndian) {
      v = java.nio.ByteBuffer.wrap(arr).getDouble
    } else {
      v = java.nio.ByteBuffer.wrap( NValueType.reverseBytes(arr) ).getDouble
    }

    value = v

    length
  }
}

object NDouble {
  def apply(s: Double) = new NDouble(s)
  def apply(s: NDouble) = new NDouble(s.value)
}