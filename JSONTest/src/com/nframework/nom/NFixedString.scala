package com.nframework.nom

import java.nio.ByteBuffer

class NFixedString(s: String, typeLen: Short, fixedLen: Int) extends NValueType {
  var value: String = s
  var fixedLength = fixedLen
  
  dataType = EDataType.FIXED_STRING
  typeLength = 2
  length = fixedLength * typeLength
  
  def this() = this("", 2, 0)
  def this(s: String, fixedLen: Int) = this(s, 2, fixedLen)
  def this(s: String) = this(s, 2, 0)
  def this(fixedLen: Int) = this("", 2, 0)
  
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
    this.value = valueType.asInstanceOf[NFixedString].value
    this.length = valueType.asInstanceOf[NFixedString].value.length() * typeLength
    
    true
  }
  
  def getClone() : NValueType = {
    val clone = new NFixedString(value)
    
    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable
    clone.fixedLength = fixedLength
    clone.typeLength = typeLength
    
    clone
  }
  
  def copyTo(to: NValueType) {
    to.asInstanceOf[NFixedString].value = value
  }
   
  def serialize() : (Array[Byte], Int) = {
    // EUC-KR 또는 UTF-16LE로만 인디코딩 한다. UTF-8은 따로 고려하지 않는다.
    val encoding = typeLength match {
      case 1 => "EUC-KR"
      case _ => "UTF-16LE"
    }
    
    val data = value.getBytes(encoding)
    val serialized = new Array[Byte](length)
    
    if(data.length <= serialized.length)
      data.copyToArray(serialized, 0)
    else
      data.copyToArray(serialized, 0, serialized.length)
    
    ( serialized, serialized.length )
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val encoding = typeLength match {
      case 1 => "EUC-KR"
      case _ => "UTF-16LE"
    }
     
    val stringValue = new Array[Byte](length)
    data.copyToArray(stringValue, offset, length)
    
    value = new String(stringValue, encoding)      
    
    length
  }
}

object NFixedString {
  def apply(s: String) = new NFixedString(s)
  def apply(len: Int) = new NFixedString(len)
  def apply(s: NFixedString) = new NFixedString(s.value)
}