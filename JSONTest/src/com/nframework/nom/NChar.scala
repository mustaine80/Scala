package com.nframework.nom

class NChar(b: Char) extends NValueType {
  var value: Char = b
  
  dataType = EDataType.CHAR
  length = 1
  typeLength = 1
  
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
    this.value = valueType.asInstanceOf[NChar].value
    
    true
  }
  
  def getClone() : NValueType = {
    val clone = new NChar(value)
    
    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable
    
    clone
  }
  
  def copyTo(to: NValueType) {
    to.asInstanceOf[NChar].value = value
  }
  
  def serialize(length: Int) : Array[Byte] = {
    Array(value.asInstanceOf[Byte])
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val length: Int = 1
    
    value = Array(offset).asInstanceOf[Char]
    
    length
  }
}

object NChar {
  def apply(c: Char) = new NChar(c)
  def apply(c: NChar) = new NChar(c.value)
}