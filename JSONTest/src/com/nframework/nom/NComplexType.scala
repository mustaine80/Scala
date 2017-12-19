package com.nframework.nom

import scala.collection.mutable._
import java.nio.ByteBuffer

class NComplexType extends NDataType {
  def getChild(idx: Int, n: Int) : NDataType = {    
    var dt: NDataType = null
    
    if(idx < children.length && idx >= 0) {
      dt = children(idx)(n)
    }
    
    dt
  }
  
  def getChildCount() : Int = {
    children(0).length
  }
  
  def getValue(n: Int) : NValueType = {
    val builder = new StringBuilder
    
    builder.append("(NComplexType::getValue) You can't get value from complex type! (")
    builder.append(path)
    builder.append(")")
    
    throw new NInvalidFieldException(builder.toString())
    
    null
  }
  
  def setValue(value: NValueType, n: Int) : Boolean = {
    val builder = new StringBuilder
    
    builder.append("(NComplexType::getValue) You can't set value to complex type! (")
    builder.append(path)
    builder.append(")")
    
    throw new NInvalidFieldException(builder.toString())
    
    false
  }
  
  def getLength() : Int = {
    var length = 0
    
    length += indicator.abs
    
    length += children.map( (list: ListBuffer[NDataType]) => { list.map( (dt: NDataType) => dt.getLength()).reduce(_ + _) } ).reduce(_ + _)
    
    length
  }
  
  def addDataType(idx: Int, dataType: NDataType) {
    children(idx) += dataType
    length += dataType.getLength()
  }
  
  def getClone() : NDataType = {
    var ct = new NComplexType
    
    ct.name = this.name
    ct.path = this.path
    ct.indicator = this.indicator
    ct.setSize(this.size)
    
    for(i <- (0 to size-1)) {
      for(j <- (0 to children(i).length-1)) {
        val childType = children(i)(j)
        val newType = childType.getClone()
        
        ct.addDataType(i, newType)
      }
    }
    
    ct
  }
  
  def isLeaf() : Boolean = {
    false
  }
  
  def setSize(n: Int) {
    resize(n)
  }
  
  def resize(n: Int) {
    
  }
  
  def setNOM(nom: NOM) {
    this.nom = nom
  }
  
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int) : Int = {
    0
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
    (0, 0)
  }
  
  def deserialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Boolean, Int, Int) = {
    (false, 0, 0)
  }
  
  def copyTo(to: NDataType) : Boolean = {
    false
  }
  
  def setOMT(b: Boolean) {
    this.omt = b
  }
  
  // normal methods
 
  
  // fields
  private var children = new ListBuffer[ListBuffer[NDataType]]
  private var length: Int = 0
  
  name = "NComplexType"
  size = 1
  indicator = 0
  
  children += new ListBuffer[NDataType]
}