package com.nframework.nom

trait NDataType {
  var name = ""
  var path = ""
  var size = 1
  var typeName = ""
  var indicator: Int = 0
  var nom: NOM = null
  
  protected var alignmentLength = 4
  protected var omt = false
  
  def getChild(idx: Int, n: Int) : NDataType
  def getChildCount() : Int
  def getValue(n: Int) : NValueType
  def setValue(value: NValueType, n: Int) : Boolean
  def getLength() : Int
  def addDataType(n: Int, dataType: NDataType)
  def getClone() : NDataType
  def isLeaf() : Boolean
  def setSize(n: Int)
  def resize(n: Int)
  
  def setNOM(nom: NOM)
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int) : Int
  def setAlignmentLength(length: Short)
  def getTypeLength() : Short
  def getMaxTypeLength() : Short
  def copyTo(to: NDataType) : Boolean
  def setOMT(b: Boolean)
  
  def serialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Int, Int)
  def deserialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Boolean, Int, Int)
  
  def serializeIndicator(data: Array[Byte], length: Int, offset: Int, alignment: Boolean) : (Int, Int) = {
    var len: Int = 0;
    var off: Int = offset;
    
    val maxTypeLength = getMaxTypeLength()
    
    if(indicator != 0) {
      val bb = java.nio.ByteBuffer.allocate(indicator.abs)    
      bb.putInt(indicator)
      
      var buffer = bb.array()
      if(indicator > 0)
        buffer = NValueType.reverseBytes(buffer)  // ByteBuffer의 결과는 big endian이므로 little인 경우에는 reverseByte
      
      buffer.copyToArray(data, offset, indicator.abs)          
    
      len = indicator.abs
      off += indicator.abs
    }
    
    // alignment는 나중에 추가한다.
    
    (len, off)
  }
  
  def deserializeIndicator(data: Array[Byte], length: Int, offset: Int, alignment: Boolean) : (Int, Int, Int) = {
    var len = 0
    var field = 0
    var off = offset
    
    val maxTypeLength = getMaxTypeLength()
    
    if(indicator != 0) {
      val arr = new Array[Byte](indicator.abs) 
      
      data.copyToArray(arr, offset, indicator.abs)
      
      field = if(indicator > 0) {
        java.nio.ByteBuffer.wrap(arr.reverse).getInt
      } else {
        java.nio.ByteBuffer.wrap(arr).getInt
      }
      
      len = indicator.abs
      off += indicator.abs
    } else {
      field = 0
    }
    
    // alignment는 나중에 추가한다.
    
    (len, off, field)
  }
}

object NDataType {
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