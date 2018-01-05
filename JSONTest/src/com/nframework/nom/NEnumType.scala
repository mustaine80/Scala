package com.nframework.nom

import scala.collection.mutable._
import java.nio.ByteBuffer

// enumerator case class
case class NEnumerator(val name: String, val value: Int)

class NEnumType extends NDataType {
  def getChild(idx: Int, n: Int) : NDataType = {    
    null
  }
  
  def getChildCount() : Int = {
    0
  }
  
  def getValue(n: Int) : NValueType = {
    if(n >= 0 && n < size) {
      enumValue.enumerator = enumList(n).name
      enumValue.enumValue = enumList(n).value
    } else {
      val builder = new StringBuilder
      builder.append("(NEnumType::getValue) ")
      builder.append(path)
      builder.append(" don't have an element at index ")
      builder.append(n)
      
      throw new NArrayIndexOutOfBoundsException(builder.toString())
    }
    
    enumValue
  }
  
  def setValue(value: NValueType, n: Int) : Boolean = {
    var success = false
    
    val enumObj: NEnum = value match {
      case v: NEnum => v
      case _ => {    
        val builder = new StringBuilder
        builder.append("(NEnumType::setValue) Type mismatched: ")
        builder.append(path)
        
        throw new NTypeMismatchException(builder.toString())
        null
      }
    }        
    
    if(value.toString().length() != 0) {
      success = setValue(value.toString(), n)
    } else {
      success = setValue(value.toInt(), n)
    }
    
    success
  }
  
  def getLength() : Int = {
    var length = indicator.abs
    length += (this.length * this.size)
    length
  }
  
  def addDataType(idx: Int, dataType: NDataType) {
    // do nothing
  }
  
  def getClone() : NDataType = {
    val et = new NEnumType()
    
    et.name = this.name
    et.path = path
    et.setSize(size)
    
    for(i <- (0 to size - 1)) et.enumList(i) = enumList(i)
    
    et.enumMap = this.enumMap.clone()
    et.enumValueMap = this.enumValueMap.clone()
    
    et
  }
  
  def isLeaf() : Boolean = {
    false
  }
  
  def setSize(n: Int) {
    resize(n)
  }
  
  def resize(n: Int) {
    if(size > n) {
      enumList.dropRight(size - n)
    } else {
      for(i <- (size to n)) enumList += enumList(0) 
    }
  }
  
  def setNOM(nom: NOM) {
    this.nom = nom
  }
  
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int) : Int = {
    var length = 0
    
    length += indicator.abs
    length += (this.length * this.size)
    
    // alignment는 나중에 처리함
    
    length
  }
  
  def setAlignmentLength(length: Short) {
    this.alignmentLength = length
  }
  
  def getTypeLength() : Short = {
    this.length.asInstanceOf[Short]
  }
  
  def getMaxTypeLength() : Short = {
    getTypeLength()
  }
  
  def serialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Int, Int) = {
    var valueLength = 0
    var value = 0
    
    var len = 0
    var off = 0
    
    val info = serializeIndicator(data, length, offset, alignment)
    
    len = info._1
    off = info._2
    
    for(i <- (0 to size-1)) {
      value = enumList(i).value
      valueLength = this.length
      
      val bb = java.nio.ByteBuffer.allocate(valueLength)
      bb.putInt(value)
      
      var buffer = bb.array()
      
      buffer.copyToArray(data, off)
      off += valueLength
      len += valueLength
    }
    
    // 구조체 멤버 맞춤 처리는 나중에...
    
    (len, off)
  }
  
  def deserialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Boolean, Int, Int) = {
    var value = 0
    var valueLength = this.length
    var fieldSize = 0
    var len = 0
    var off = 0
    
    val info = deserializeIndicator(data, length, offset, alignment)
    
    len = info._1
    off = info._2
    fieldSize = info._3
    
    if(fieldSize != 0) {
      resize(fieldSize)
    }
    
    for(i <- (0 to size-1)) {    
      val arr = new Array[Byte](valueLength) 
        
      data.copyToArray(arr, off, len)
           
      value = java.nio.ByteBuffer.wrap(arr.reverse).getInt
      
      val e = enumValueMap.get(value)
      
      enumList(i) = e match {
        case Some(v) => v
        case None => NEnumerator("", 0)
      }
      
      off += valueLength
      len += valueLength
    }
    
    // alignment 구현은 나중에...
    
    (true, len, off)
  }
  
  def copyTo(to: NDataType) : Boolean = {
    var success = false
    
    to match {
      case et: NEnumType => {
        if(this.size != et.size)
          et.resize(this.size)
          
        for(i <- (0 to size-1)) et.enumList(i) = this.enumList(i)
        
        success = true
      }
      case _ => 
    }
    
    success
  }
  
  def setOMT(b: Boolean) {
    this.omt = b
  }
  
  // normal methods
  private def setValue(enumerator: String, n: Int) : Boolean = {
    var success = false
    
    enumMap.get(enumerator) match {
      case Some(e) => {        
        if(n >= 0 && n < size) {
          enumList(n) = e
          
          success = true
        } else {
          val builder = new StringBuilder
          builder.append("(NEnumType::setValue) ");
          builder.append(path)
          builder.append(" don't have an element at index ")
          builder.append(n)
          
          throw new NArrayIndexOutOfBoundsException(builder.toString)
        }
        
      }
      case None => // do nothing 
    }
    
    success
  }
  
  private def setValue(enumValue: Int, n: Int) : Boolean = {
    var success = false
    
    enumValueMap.get(enumValue) match {
      case Some(e) => {        
        if(n >= 0 && n < size) {
          enumList(n) = e
          
          success = true
        } else {
          val builder = new StringBuilder
          builder.append("(NEnumType::setValue) ");
          builder.append(path)
          builder.append(" don't have an element at index ")
          builder.append(n)
          
          throw new NArrayIndexOutOfBoundsException(builder.toString)
        }
        
      }
      case None => // do nothing 
    }
    
    success
  }
  
  def addEnumerator(name: String, value: Int) {
    val e = NEnumerator(name, value)
    
    enumMap += (name -> e)
    enumValueMap += (value -> e)
  }
  
  // fields
  private var enumValue = NEnum("", 0)    
  private var enumMap = collection.mutable.HashMap.empty[String, NEnumerator]
  private var enumValueMap = collection.mutable.HashMap.empty[Int, NEnumerator]
  var length: Int = 0
  var enumList: ListBuffer[NEnumerator] = new ListBuffer
    
  size = 1
  indicator = 0
}