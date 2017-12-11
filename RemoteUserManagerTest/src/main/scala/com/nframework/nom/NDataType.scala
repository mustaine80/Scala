package com.nframework.nom

sealed abstract class NOM

case class DummyNOM(objName: String, value: NChar_Dummy) extends NOM

case class NChar_Dummy(value: Char) extends NOM with NValueType {
  def this() = this(' ')
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
    true
  }

  def getClone() : NValueType = {
    NChar_Dummy(0x5)
  }

  def copyTo(to: NValueType) {
    to.asInstanceOf[NChar].value = value
  }

  def serialize(length: Int) : Array[Byte] = {
    Array(value.asInstanceOf[Byte])
  }

  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val length: Int = 1
    length
  }
}

trait NDataType {
  var name = ""
  var path = ""
  var size = 1
  var typeName = ""
  var indicator: Int = 0
  var nom: NOM = null

  protected var alignmentLength = 4
  protected var omt = false

  def getChild(idx: Int, n: Int) : NDataType
  def getChildCount() : Int
  def getValue(n: Int) : NValueType
  def setValue(value: NValueType, n: Int) : Boolean
  def getLength() : Int
  def addDataType(n: Int, dataType: NDataType)
  def getClone() : NDataType
  def isLeaf() : Boolean
  def setSize(n: Int)
  def resize(n: Int)

  def setNOM(nom: NOM)
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int)
  def setAlignmentLength(length: Short)
  def getTypeLength() : Short
  def getMaxTypeLEngth() : Short
  def copyTo(to: NDataType) : Boolean
  def setOMT(b: Boolean)

  def serialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short)
  def deserialize(data: Array[Byte], length: Int, offset: Boolean, nextTypeLength: Short) : Boolean
}

object NDataType {
  def reverseBytes(input: Array[Byte]) : Array[Byte] = {
    input.reverse
  }

  def reverseBytes(input: Array[Byte], size: Int) : Array[Byte] = {
    input.dropRight(input.length - size).reverse ++ input.drop(size)
  }

  def reverseBytes(input: Array[Byte], offset: Int, size: Int) : Array[Byte] = {
    input.dropRight(input.length - offset) ++ input.drop(offset).dropRight(input.length - offset - size).reverse ++ input.drop(offset + size)
  }

  def serializeIndicator(data: Array[Byte], length: Int, offset: Int, alignment: Boolean) {

  }

  def deserializeIndicator(data: Array[Byte], length: Int, fieldSize: Int, offset: Int, alignment: Boolean) = {


    false
  }
}
