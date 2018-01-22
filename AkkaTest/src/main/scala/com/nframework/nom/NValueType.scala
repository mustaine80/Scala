package com.nframework.nom

sealed abstract class NOM

/**
  *
  * @param name Pub/Sub 통신 간 사용할 메시지 이름
  * @param objID  Object type 에 대해 개별 객체 구분을 위해 사용
  * @param data 직렬화 데이터
  */
case class NMessage(name: String, objID: Int, data: Array[Byte]) extends NOM

object EDataType extends Enumeration {
  val INVALID = Value(0)
  val BOOL = Value
  val BYTE = Value
  val CHAR = Value
  val SHORT = Value
  val INTEGER = Value
  val UNSIGNED_SHORT = Value
  val UNSIGNED_INTEGER = Value
  val FLOAT = Value
  val DOUBLE = Value
  val STRING = Value
  val VARIABLE = Value
  val FIXED_STRING = Value
  val FIXED_DATUM = Value
  val ENUM = Value
}


trait NValueType {
  var bigEndian = false
  var signed = true
  var length = 0
  var variable = false
  var typeLength = 0
  var dataType = EDataType.INVALID

  def toInt(): Int
  def toShort(): Short
  def toChar(): Char
  def toByte(): Byte
  def toFloat(): Float
  def toDouble(): Double
  def toString(): String

  def setValue(valueType: NValueType) : Boolean
  def getClone() : NValueType
  def copyTo(to: NValueType)
  def serialize() : (Array[Byte], Int)
  def deserialize(data: Array[Byte], offset: Int) : Int
}

object NValueType {
  def reverseBytes(input: Array[Byte]) : Array[Byte] = {
    input.reverse
  }

  def reverseBytes(input: Array[Byte], size: Int) : Array[Byte] = {
    input.dropRight(input.length - size).reverse ++ input.drop(size)
  }

  def reverseBytes(input: Array[Byte], offset: Int, size: Int) : Array[Byte] = {
    input.dropRight(input.length - offset) ++ input.drop(offset).dropRight(input.length - offset - size).reverse ++ input.drop(offset + size)
  }
}

