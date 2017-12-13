package com.nframework.nom

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
  var path = ""
  var nom: NOM = null
  
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

