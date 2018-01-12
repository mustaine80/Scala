package com.nframework.nom

import java.nio.ByteBuffer

class NVariable(v: Array[Byte], len: Int) extends NValueType {
  var value: Array[Byte] = v
  
  dataType = EDataType.VARIABLE
  length = 4 + len
  variable = true
  
  def this() = this(null, 2)
  def this(v: Array[Byte]) = this(v, v.length)
  def this(len: Int) = this(new Array[Byte](len), len)
  
  def toInt(): Int = {
    0
  }
  
  def toShort(): Short = {
    0
  }
  
  def toChar(): Char = {
    0
  }
  
  def toByte(): Byte = {
    0
  }
  
  def toFloat(): Float = {
    0
  }
  
  def toDouble(): Double = {
    0
  }
  
  override def toString(): String = {
    ""
  }
  
  def setValue(valueType: NValueType) : Boolean = {
    var success = false
    
    valueType match {
      case v: NVariable => {
        value = new Array[Byte](v.value.length)
        v.value.copyToArray(value)
        length = 4 + value.length
        success = true
      }
    }
    
    success
  }
  
  def getClone() : NValueType = {
    val clone = new NVariable(value)
    
    value.copyToArray(clone.value)
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable
    
    clone
  }
  
  def copyTo(to: NValueType) {
    to match {
      case v: NVariable => {
        v.value = new Array[Byte](value.length)
        value.copyToArray(v.value)
      }
    }
  }
   
  def serialize() : (Array[Byte], Int) = {
    val serialized = new Array[Byte](length)
    val datumLength = value.length
    
    val bb = java.nio.ByteBuffer.allocate(4)
    bb.putInt(datumLength)
    
    bb.array().reverse.copyToArray(serialized, 0)
    
    value.copyToArray(serialized, 4)
    
    ( serialized, length )
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {    
    val indicator = new Array[Byte](4)
    
    data.drop(offset).copyToArray(indicator, 0, 4)
    
    val valueLength = java.nio.ByteBuffer.wrap(indicator.reverse).getInt
    
    value = new Array[Byte](valueLength)
    data.drop(offset + 4).copyToArray(value, 0, valueLength)
    
    length = 4 + valueLength
    
    length
  }
}

object NVariable {
  def apply(s: Array[Byte]) = new NVariable(s)
  def apply(len: Int) = new NVariable(len)
  def apply(s: NVariable) = new NVariable(s.value)
}
