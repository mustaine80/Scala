package com.nframework.nom

import scala.collection.mutable._

final class NField(field: NField) {
  // properties
  private var _dataType: NDataType = null
  private var _alignmentLength : Short = 0
  
  var size = 1
  var length = 0
  var name = ""
  var note = ""
  var indicator = 0
  var fixedLength = 0
  var alignment = false
  var typeName = ""
  var omt = false
  var nextField: NField = null
  
  // setter
  def dataType_=(value: NDataType) {
    _dataType = value
    
    if(_dataType != null)
      _dataType.setSize(size)      
  }
  def alignmentLength_=(value: Short) {
    _alignmentLength = value
    
    if(_dataType != null)
      _dataType.setAlignmentLength(value)
  }
  
  // getter
  def dataType = _dataType
  def alignmentLength = _alignmentLength
  
  // constructor
  def this() = this(null)
  
  if(field != null) {
    name = field.name
    note = field.note
    size = field.size
    indicator = field.indicator
    fixedLength = field.fixedLength
    dataType = field.dataType.getClone()
  }
  
  // normal methods
  def getLength() : Int = {
    var length = 0
    var nextTypeLength: Short = 0
    
    length = alignment match {
      case true => {
        if(nextField != null)
          nextTypeLength = nextField.getTypeLength()
        else nextTypeLength = 0
        
        getLength(alignment, nextTypeLength, 0)
      }
      case false => dataType.getLength()
    }
    
    this.length = length
    
    this.length
  }
  
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int) = {
    var length = 0
    
    if(this.length == 0) {
      length = dataType.getLength(alignment, nextTypeLength, offset)
      this.length = length
    }
    
    this.length
  }
  
  def setOMT(b: Boolean) {
    if(dataType != null)
      dataType.setOMT(b)
  }
  
  def getTypeLength() : Short = {
    dataType.getTypeLength()
  }
  
  def getMaxTypeLength() : Short = {
    dataType.getMaxTypeLength()
  }
  
  def serialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Int, Int) = {
    dataType.serialize(data, length, offset, alignment, nextTypeLength)
  }
  
  def deserialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short) : (Boolean, Int, Int) = {
    dataType.deserialize(data, length, offset, alignment, nextTypeLength)
  }
  
  def copyTo(to: NField) : Boolean = {
    var success = false
    
    var typeFrom = this.dataType
    var typeTo = to.dataType
    
    if(typeFrom != null && typeTo != null) {
      success = typeFrom.copyTo(typeTo)
      to.length = this.length
    }
    
    success
  }
}