package com.nframework.nom

class NBool(b: Boolean) extends NValueType {
  var value: Boolean = b
  
  dataType = EDataType.BOOL
  length = 1
  typeLength = 1
  
  def this() = this(false)
  
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
    this.value = valueType.asInstanceOf[NBool].value
    
    true
  }
  
  def getClone() : NValueType = {
    val clone = new NBool(value)
    
    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable
    
    clone
  }
  
  def copyTo(to: NValueType) {
    to.asInstanceOf[NBool].value = value
  }
  
  def serialize() : (Array[Byte], Int) = {
    ( Array(value.asInstanceOf[Byte]), 1 )
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val length: Int = 1
    
    value = Array(offset).asInstanceOf[Boolean]
    
    length
  }
}

object NBool {
  def apply(b: Boolean) = new NBool(b)
  def apply(b: NBool) = new NBool(b.value)
}