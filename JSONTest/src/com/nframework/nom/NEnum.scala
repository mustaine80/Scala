package com.nframework.nom

class NEnum(var enumerator: String, var enumValue: Int) extends NValueType {  
  dataType = EDataType.ENUM
  length = 4
  typeLength = 4
  
  def this() = this("", 0)
  
  def this(enum: String) = this(enum, 0)
  
  def this(enum: Int) = this("", enum)
  
  def toInt(): Int = {
    enumValue
  }
  
  def toShort(): Short = {
    enumValue.asInstanceOf[Short]
  }
  
  def toChar(): Char = {
    enumValue.asInstanceOf[Char]
  }
  
  def toByte(): Byte = {
    enumValue.asInstanceOf[Byte]
  }
  
  def toFloat(): Float = {
    enumValue.asInstanceOf[Float]
  }
  
  def toDouble(): Double = {
    enumValue.asInstanceOf[Double]
  }
  
  override def toString(): String = {
    enumerator
  }
  
  def setValue(valueType: NValueType) : Boolean = {
    this.enumValue = valueType.asInstanceOf[NEnum].enumValue
    this.enumerator = valueType.asInstanceOf[NEnum].enumerator
    
    true
  }
  
  def getClone() : NValueType = {
    val clone = new NEnum(enumerator, enumValue)
    
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable
    
    clone
  }
  
  def copyTo(to: NValueType) {
    to.asInstanceOf[NEnum].enumValue = enumValue
    to.asInstanceOf[NEnum].enumerator = enumerator
  }
  
  def serialize() : (Array[Byte], Int) = {
    // enum은 serialize를 지원하지 않음. NEnumType에서 처리함
    
    ( null, 0 )
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {
    // enum은 deserialize를 지원하지 않음. NEnumType에서 처리함
    
    0
  }
}

object NEnum {
  def apply(enumerator: String, enumValue: Int) = new NEnum(enumerator, enumValue)
  def apply(e: NEnum) = new NEnum(e.enumerator, e.enumValue)
  def apply(enumerator: String) = new NEnum(enumerator)
  def apply(enumValue: Int) = new NEnum(enumValue)
}