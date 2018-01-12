package com.nframework.nom

import scala.collection.mutable._
import java.nio.ByteBuffer

class NComplexType extends NDataType {
  def getChild(idx: Int, n: Int) : NDataType = {    
    var dt: NDataType = null
    
    if(idx < children.length && idx >= 0 && n < children(idx).length) {
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
    val builder = new StringBuilder
    
    if(size > n) {
      for(i <- (size + 1 to n)) {
        for(j <- (0 to children(i).length - 1)) {
          if(nom != null)
            nom.deleteDataTypeMapElement(children(i)(j).path)
        }
      }
      
      children = children.dropRight(size - n)
      
    } else if(size < n) {      
      for(i <- (size to n - 1)) {
        val typeList = ListBuffer.empty[NDataType]
        
        for(j <- (0 to children(0).length -1)) {
          val toAdd = children(0)(j).getClone()
          typeList += toAdd
          
          val pathBuilder = new StringBuilder
          pathBuilder.append(path)
          pathBuilder.append("[")
          pathBuilder.append(i)
          pathBuilder.append("]")
          
          if(nom != null)
            nom.addDataTypeMapElement(pathBuilder.toString(), toAdd)
            
          pathBuilder.append(".")
          pathBuilder.append(toAdd.name)
          toAdd.path = pathBuilder.toString()
        }
      }
      
      if(size == 1 && n > 1) {
        for(j <- (0 to children(0).length - 1)) {
          val pathBuilder = new StringBuilder
          pathBuilder.append(path)
          pathBuilder.append("[0]")
          
          if(nom != null)
            nom.addDataTypeMapElement(pathBuilder.toString(), children(0)(j))
            
          pathBuilder.append(".")
          pathBuilder.append(children(0)(j).name)
          children(0)(j).path = pathBuilder.toString()
        }
      }
      
    } else {
      
    }
    
    size = n
  }
  
  def setNOM(nom: NOM) {
    for(i <- (0 to children.length - 1))
      for(j <- (0 to children(i).length - 1))
        children(i)(j).setNOM(nom)
        
    this.nom = nom
  }
  
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int) : Int = {
    var valueLength = 0
    var len = 0
    var off = offset
    var next = nextTypeLength
    var oldNext = next
    
    len += indicator.abs
    off += len
    
    for(i <- (0 to children.length - 1)) {
      for(j <- (0 to children(i).length - 1)) {
        if(j != children(i).length - 1) {
          next = children(i)(j + 1).getTypeLength()
        } else {
          next = oldNext
        }
        
        valueLength = children(i)(j).getLength(alignment, next, off)
        len += valueLength
        off += valueLength
      }
    }
    
    length = len
    
    length
  }
  
  def setAlignmentLength(length: Short) {
    this.alignmentLength = length
    
    for(i <- (0 to children.length - 1))
      for(j <- (0 to children(i).length - 1))
        children(i)(j).setAlignmentLength(length)   
  }
  
  def getTypeLength() : Short = {
    val t = children(0)(0)
    var typeLength: Short = 0
    
    typeLength = t match {
      case null => 0
      case _ => t.getTypeLength()
    }
    
    typeLength
  }
  
  def getMaxTypeLength() : Short = {
    var len: Short = 0
    
    len = children.map( (l: ListBuffer[NDataType]) => l.map(_.getMaxTypeLength()).reduceLeft(_ max _)).reduceLeft(_ max _)
    
    len
  }
  
  def serialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Int, Int) = {
    var valueLength = 0
    var oldNext = nextTypeLength
    var next = nextTypeLength
    
    var len = 0
    var off = offset
    
    val indicatorInfo = serializeIndicator(data, length, offset, alignment)
    
    len = indicatorInfo._1
    off = indicatorInfo._2
    
    for(i <- (0 to children.length - 1)) {
      for(j <- (0 to children(i).length - 1)) {
        if(j != children(i).length - 1)
          next = children(i)(j + 1).getTypeLength()
        else
          next = oldNext
          
        val info = children(i)(j).serialize(data, len, off, alignment, next)
        valueLength = info._1
        
        len += valueLength
        off = info._2
      }
    }
    
    (len, off)
  }
  
  def deserialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Boolean, Int, Int) = {
    var valueLength = 0
    var oldNext = nextTypeLength
    var next = nextTypeLength
    
    var len = 0
    var off = offset
    var fieldSize = 0
    
    val indicatorInfo = deserializeIndicator(data, length, offset, alignment)
    len = indicatorInfo._1
    off = indicatorInfo._2
    fieldSize = indicatorInfo._3
    
    if(fieldSize != 0)
      resize(fieldSize)
      
    for(i <- (0 to children.length - 1)) {
      for(j <- (0 to children(i).length - 1)) {
        if(j != children(i).length - 1)
          next = children(i)(j + 1).getTypeLength()
        else
          next = oldNext
          
        val info = children(i)(j).deserialize(data, len, off, alignment, next)
        len += info._2
        off = info._3
      }
    }
    
    (true, len, off)
  }
  
  def copyTo(to: NDataType) : Boolean = {
    var success = false
    
    val ct = to.asInstanceOf[NComplexType]
    
    if(ct != null) {
      if(this.size != ct.size)
        ct.resize(this.size)
        
      for(i <- (0 to children.length - 1)) {
        for(j <- (0 to children(i).length - 1)) {
          val child = ct.getChild(i, j)
          children(i)(j).copyTo(child)
        }
      }
      
      success = true
    }
    
    success
  }
  
  def setOMT(b: Boolean) {
    this.omt = b
    
    for(i <- (0 to children.length - 1)) 
        for(j <- (0 to children(i).length - 1)) 
          children(i)(j).setOMT(b)
  }
  
  // normal methods
  def reconstruct() {
    for(i <- (1 to children.length - 1)) 
        for(j <- (0 to children(0).length - 1))
          children(i) += children(0)(j).getClone()
  }
 
  
  // fields
  private var children = new ListBuffer[ListBuffer[NDataType]]
  private var length: Int = 0
  
  name = "NComplexType"
  size = 0
  indicator = 0
  
  children += new ListBuffer[NDataType]
}