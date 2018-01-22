package com.nframework.nom

class NShort(s: Short) extends NValueType {
  var value: Short = s

  dataType = EDataType.SHORT
  length = 2
  typeLength = 2

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
    this.value = valueType.asInstanceOf[NShort].value

    true
  }

  def getClone() : NValueType = {
    val clone = new NShort(value)

    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable

    clone
  }

  def copyTo(to: NValueType) {
    to.asInstanceOf[NShort].value = value
  }

  def serialize() : (Array[Byte], Int) = {
    val bb = java.nio.ByteBuffer.allocate(length)

    bb.putInt(value)

    var buffer = bb.array()

    if(bigEndian) {

    } else {
      // java.nio.ByteBuffer는 Big Endian 인디코딩이 기본 설정이므로 value가 little endian인 경우는 byte 순서를 바꿔줘야 함
      buffer = NValueType.reverseBytes(buffer)
    }

    ( buffer.toArray, length )
  }

  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val length: Int = 2
    var v: Short = 0

    val arr = new Array[Byte](length)

    data.copyToArray(arr, offset, length)

    if(bigEndian) {
      // java.nio.ByteBuffer 기본 설정이 big endian이므로 little일 경우에만 byte 배열을 뒤집어서 디코딩한다.
      v = java.nio.ByteBuffer.wrap(arr).getShort
    } else {
      v = java.nio.ByteBuffer.wrap( NValueType.reverseBytes(arr) ).getShort
    }

    value = v

    length
  }
}

object NShort {
  def apply(s: Short) = new NShort(s)
  def apply(s: NShort) = new NShort(s.value)
}