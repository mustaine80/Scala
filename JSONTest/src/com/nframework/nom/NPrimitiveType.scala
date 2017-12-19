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
  
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int) : Int = {
    var length = 0
    var value: NValueType = null
    var alignmentLength: Short = 0
    
    length += math.abs(indicator)
    
    length += valueList.map(_.length).reduce(_ + _)
    
	// 구조체 멤버 맞춤 기능은 나중에...
    
    length
  }
  
  def setAlignmentLength(length: Short) {
    this.alignmentLength = length
  }
  
  def getTypeLength() : Short = {
    valueList(0).length.asInstanceOf[Short]
  }
  
  def getMaxTypeLength() : Short = {
    if(valueList(0).variable) {
      valueList.map(_.length).reduceLeft(_ max _).asInstanceOf[Short]
    } else {
      getTypeLength()
    }
  }
  
  def copyTo(to: NDataType) : Boolean = {
    val pt = to.asInstanceOf[NPrimitiveType]
    
    if(this.size != pt.size)
      pt.resize(this.size)
      
    copyValue(pt)
    
    true
  }
  
  def setOMT(b: Boolean) {
    this.omt = b
  }
    
  def serialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Int, Int) = {
    val indicatorInfo = serializeIndicator(data, length, offset, alignment)
    var len = length + indicatorInfo._1
    var off = length + indicatorInfo._2
    
    valueList.foreach( (v: NValueType) => { 
        val info = v.serialize()
        info._1.copyToArray(data, off)
        off += info._2
        len += info._2
      }
    )
    
    // alignment는 나중에 처리한다.
    
    (len, off)
  }
  
  def deserialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Boolean, Int, Int) = {
    val indicatorInfo = deserializeIndicator(data, length, offset, alignment)
    val fieldSize = indicatorInfo._3

    var len = length
    var off = offset
    
    if(fieldSize != 0) {
      resize(fieldSize)
    }
    
    valueList.foreach( (v: NValueType) => {
      val valueLength = v.deserialize(data, offset)
      
      off += valueLength
      len += valueLength
    })
    
    // alignment는 나중에 처리한다.
    
    (true, len, off)
  }
  
  // normal methods
  def setValueObject(obj: NValueType) {
    if(size != 0) {
      valueList(0) = obj
    } else {
      valueList += obj
      size = 1
    }
    
    valueList(0).path = path
    
    for( i <- (1 to size - 1) ) valueList(i) = obj.getClone()
  }
  
  def copyValue(pt: NPrimitiveType) {
    for( i <- (0 to valueList.size - 1) ) valueList(i).copyTo(pt.valueList(i))
  }
  
  
  // fields
  private var valueList: ListBuffer[NValueType] = new ListBuffer
  
  name = "NPrimitiveType"
  size = 0
  indicator = 0
}