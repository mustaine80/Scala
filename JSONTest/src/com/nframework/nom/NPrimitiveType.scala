package com.nframework.nom

import scala.collection.mutable._

class NPrimitiveType extends NDataType {
  def getChild(idx: Int, n: Int) : NDataType = {    
    null
  }
  
  def getChildCount() : Int = {
    0
  }
  
  def getValue(n: Int) : NValueType = {
    var value: NValueType = null
    
    value = if(n >= 0 && n < size) valueList(n) else {
      val builder = new StringBuffer
      
      builder.append("(NPrimitiveType::getValue) '")
      builder.append(path)
      builder.append("' don't have an element at index")
      builder.append(n)
      
      throw new NArrayIndexOutOfBoundsException(builder.toString())
      
      null
    }   
    
    value
  }
  
  def setValue(value: NValueType, n: Int) : Boolean = {
    if(n < 0 || n >= valueList.length) {
      val builder = new StringBuffer
      
      builder.append("(NPrimitiveType::setValue) '")
      builder.append(path)
      builder.append("' don't have an element at index")
      builder.append(n)
      
      throw new NArrayIndexOutOfBoundsException(builder.toString())
    }
    
    var success = valueList(n).setValue(value)
    
    if(!success) {
      val builder = new StringBuffer
      
      builder.append("(NPrimitiveType::setValue) Type mismatched: ")
      builder.append(path)
            
      throw new NTypeMismatchException(builder.toString())
    }
    
    success
  }
  
  def getLength() : Int = {
    var len: Int = scala.math.abs(indicator)
    
    len += valueList.map(_.length).reduce(_ + _)
    
    len
  }
  
  def addDataType(n: Int, dataType: NDataType) {
    // do nothing
  }
  
  def getClone() : NDataType = {
    val newType = new NPrimitiveType
    
    newType.setValueObject(valueList(0).getClone())
    newType.setSize(size)
    newType.name = name
    newType.path = path
    newType.indicator = indicator
    
    newType.valueList = valueList.map(_.getClone())
    
    newType
  }
  
  def isLeaf() : Boolean = {
    true
  }
  
  def setSize(n: Int) {
    resize(n)
  }
  
  def resize(n: Int) {
    if(size > n) {
      valueList.dropRight(size - n)
    } else if(size < n) {
      for(i <- 0 to (n - size)) valueList += valueList(0).getClone()
    } else {
      // do nothing
    }
  }
  
  def setNOM(nom: NOM) {
    valueList.foreach(_.nom = nom)
    this.nom = nom
  }
  
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int) {
    var length = 0
    var value: NValueType = null
    var alignmentLength: Short = 0
    
    length += math.abs(indicator)
    
    length += valueList.map(_.length).reduce(_ + _)
    
    // ±¸Á¶Ã¼ ¸â¹ö ¸ÂÃã Ã³¸®´Â ³ªÁß¿¡...
  }
  
  def setAlignmentLength(length: Short) {
    this.alignmentLength = length
  }
  
  def getTypeLength() : Short = {
    0
  }
  
  def getMaxTypeLEngth() : Short = {
    0
  }
  
  def copyTo(to: NDataType) : Boolean = {
    false
  }
  
  def setOMT(b: Boolean) {
    
  }
  
  
  def serialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) {
    
  }
  
  def deserialize(data: Array[Byte], length: Int, offset: Boolean, nextTypeLength: Short) : Boolean = {
    false
  }
  
  // normal methods
  def setValueObject(obj: NValueType) {
    
  }
  
  
  // fields
  private var valueList: ListBuffer[NValueType] = new ListBuffer
  
  name = "NPrimitiveType"
  size = 0
  indicator = 0
}
