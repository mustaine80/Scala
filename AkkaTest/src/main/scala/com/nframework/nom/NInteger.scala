package com.nframework.nom

class NInteger(s: Integer) extends NValueType {
  var value: Integer = s
  
  dataType = EDataType.INTEGER
  length = 4
  typeLength = 4
  
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
    this.value = valueType.asInstanceOf[NInteger].value
    
    true
  }
  
  def getClone() : NValueType = {
    val clone = new NInteger(value)
    
    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable
    
    clone
  }
  
  def copyTo(to: NValueType) {
    to.asInstanceOf[NInteger].value = value
  }
  
  def serialize() : (Array[Byte], Int) = {
    //val buffer = new scala.collection.mutable.ArrayBuffer[Byte]
    val bb = java.nio.ByteBuffer.allocate(length)
    
    bb.putInt(value)
    
    var buffer = bb.array()
    
    if(bigEndian) {
      
    } else {
      // java.nio.ByteBuffer는 Big Endian 인디코딩이 기본 설정이므로 value가 little endian인 경우는 byte 순서를 바꿔줘야 함
      buffer = NValueType.reverseBytes(buffer)
    }
    
    ( buffer.toArray, length )
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {
    var v: Integer = 0
    
    val arr = new Array[Byte](length)

    //  todo: 일단 수정한다. copyToArray 구현을 이용할 경우 NMessage deserialize 가 불가능하다.
    //    data.copyToArray(arr, offset, length)
    Array.copy(data, offset, arr, 0, length)
    
    if(bigEndian) {
      v = java.nio.ByteBuffer.wrap(arr).getInt
    } else {
      v = java.nio.ByteBuffer.wrap( NValueType.reverseBytes(arr) ).getInt
    }
    
    value = v
    
    length
  }
}

object NInteger {
  def apply(s: Integer) = new NInteger(s)
  def apply(s: NInteger) = new NInteger(s.value)
}