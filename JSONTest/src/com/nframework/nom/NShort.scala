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
  
  def serialize(length: Int) : Array[Byte] = {
    val buffer = new scala.collection.mutable.ArrayBuffer[Byte]
    
    if(bigEndian) {
      buffer += (value / 0x100).asInstanceOf[Byte]
      buffer += (value % 0x100).asInstanceOf[Byte]
    } else {
      buffer += (value % 0x100).asInstanceOf[Byte]
      buffer += (value / 0x100).asInstanceOf[Byte]
    }
    
    buffer.toArray
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val length: Int = 2
    var v: Short = 0
    
    if(bigEndian) {
      v = (data(0).toInt * 0x100 + data(1).toInt).toShort
    } else {
      v = (data(1).toInt * 0x100 + data(0).toInt).toShort
    }
    
    value = v
    
    length
  }
}

object NShort {
  def apply(s: Short) = new NShort(s)
  def apply(s: NShort) = new NShort(s.value)
}