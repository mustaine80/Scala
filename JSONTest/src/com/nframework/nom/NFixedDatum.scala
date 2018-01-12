package com.nframework.nom

import java.nio.ByteBuffer

class NFixedDatum(d: Array[Byte], fixedLen: Int) extends NValueType {
  var value: Array[Byte] = d
  var fixedLength = fixedLen
  
  dataType = EDataType.FIXED_DATUM
  length = fixedLength
  
  def this() = this(null, 0)
  def this(fixedLen: Int) = this(new Array[Byte](fixedLen), fixedLen)
  def this(d: Array[Byte]) = this(d, d.length)
  
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
    var success = true
    
    valueType match {
      case fd: NFixedDatum => if(fd.fixedLength == fixedLength) fd.value.copyToArray(value) else success = false
      case _ => success = false
    }
    
    success
  }
  
  def getClone() : NValueType = {
    val clone = new NFixedDatum(fixedLength)
    
    this.value.copyToArray(clone.value)
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable
    clone.fixedLength = fixedLength
    clone.typeLength = typeLength
    
    clone
  }
  
  def copyTo(to: NValueType) {
    to.setValue(this)
    to.bigEndian = bigEndian
  }
   
  def serialize() : (Array[Byte], Int) = {    
    ( value, fixedLength )
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {    
    data.drop(offset).copyToArray(value, 0, length)
    
    length
  }
}

object NFixedDatum {
  def apply(s: Array[Byte]) = new NFixedDatum(s)
  def apply(len: Int) = new NFixedDatum(len)
  def apply(s: NFixedDatum) = new NFixedDatum(s.value)
}
