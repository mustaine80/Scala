package com.nframework.nom


class NByte(b: Byte) extends NValueType {
  var value: Byte = b

  dataType = EDataType.BYTE
  length = 1
  typeLength = 1

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
    this.value = valueType.asInstanceOf[NByte].value

    true
  }

  def getClone() : NValueType = {
    val clone = new NByte(value)

    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable

    clone
  }

  def copyTo(to: NValueType) {
    to.asInstanceOf[NByte].value = value
  }

  def serialize() : (Array[Byte], Int) = {
    ( Array(value.asInstanceOf[Byte]), 1)
  }

  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val length: Int = 1

    value = Array(offset).asInstanceOf[Byte]

    length
  }
}

object NByte {
  def apply(b: Byte) = new NByte(b)
  def apply(b: NByte) = new NByte(b.value)
}