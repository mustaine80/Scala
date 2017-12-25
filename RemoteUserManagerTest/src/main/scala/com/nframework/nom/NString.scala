package com.nframework.nom

class NString(s: String, typeLen: Short) extends NValueType {
  var value: String = s

  dataType = EDataType.STRING
  typeLength = typeLen
  length = 4 + s.length() * typeLength
  variable = true

  def this() = this("", 2)
  def this(s: String) = this(s, 2)

  def toInt(): Int = {
    value.toInt
  }

  def toShort(): Short = {
    value.toShort
  }

  def toChar(): Char = {
    value(0)
  }

  def toByte(): Byte = {
    value.toShort.toByte
  }

  def toFloat(): Float = {
    value.toDouble.toFloat
  }

  def toDouble(): Double = {
    value.toDouble
  }

  override def toString(): String = {
    value
  }

  def setValue(valueType: NValueType) : Boolean = {
    this.value = valueType.asInstanceOf[NString].value
    this.length = valueType.asInstanceOf[NString].value.length() * typeLength + 4

    true
  }

  def getClone() : NValueType = {
    val clone = new NString(value)

    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable

    clone
  }

  def copyTo(to: NValueType) {
    to.asInstanceOf[NString].value = value
  }

  def serialize() : (Array[Byte], Int) = {
    // EUC-KR 또는 UTF-16LE로만 인디코딩 한다. UTF-8은 따로 고려하지 않는다.
    val encoding = typeLength match {
      case 1 => "EUC-KR"
      case _ => "UTF-16LE"
    }

    val data = value.getBytes(encoding)
    val serialized = new Array[Byte](data.length + 4)

    val bb = java.nio.ByteBuffer.allocate(length)

    bb.putInt(length)

    bb.array().copyToArray(serialized, 0)
    data.copyToArray(serialized, 4)

    ( serialized, serialized.length )
  }

  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val indicator = new Array[Byte](4)
    var stringLength = 0
    val encoding = typeLength match {
      case 1 => "EUC-KR"
      case _ => "UTF-16LE"
    }

    data.copyToArray(indicator, offset, 4)

    stringLength = java.nio.ByteBuffer.wrap(indicator.reverse).getInt

    val stringValue = new Array[Byte](stringLength * typeLength)
    data.copyToArray(stringValue, offset + 4, stringValue.length)

    value = new String(stringValue, encoding)

    length = stringValue.length + 4

    length
  }
}

object NString {
  def apply(s: String) = new NString(s)
  def apply(s: NString) = new NString(s.value)
}