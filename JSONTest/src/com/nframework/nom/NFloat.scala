package com.nframework.nom

import java.nio.ByteBuffer

class NFloat(s: Float) extends NValueType {
  var value: Float = s
  
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
    this.value = valueType.asInstanceOf[NFloat].value
    
    true
  }
  
  def getClone() : NValueType = {
    val clone = new NFloat(value)
    
    clone.value = value
    clone.bigEndian = bigEndian
    clone.signed = signed
    clone.length = length
    clone.variable = variable
    
    clone
  }
  
  def copyTo(to: NValueType) {
    to.asInstanceOf[NFloat].value = value
  }
  
  def serialize() : (Array[Byte], Int) = {
    //val buffer = new scala.collection.mutable.ArrayBuffer[Byte]
    val bb = java.nio.ByteBuffer.allocate(length)
    
    bb.putFloat(value)
    
    var buffer = bb.array()
    
    if(bigEndian) {
      
    } else {
      // java.nio.ByteBuffer는 Big Endian 인디코딩이 기본 설정이므로 value가 little endian인 경우는 byte 순서를 바꿔줘야 함
      buffer = NValueType.reverseBytes(buffer)
    }
    
    ( buffer.toArray, length )
  }
  
  def deserialize(data: Array[Byte], offset: Int) : Int = {
    val length: Int = 4
    var v: Float = 0
    
    val arr = new Array[Byte](length) 
      
    data.copyToArray(arr, offset, length)
    
    if(bigEndian) {
      v = java.nio.ByteBuffer.wrap(arr).getFloat
    } else {
      v = java.nio.ByteBuffer.wrap( NValueType.reverseBytes(arr) ).getFloat
    }
    
    value = v
    
    length
  }
}

object NFloat {
  def apply(s: Float) = new NFloat(s)
  def apply(s: NFloat) = new NFloat(s.value)
}