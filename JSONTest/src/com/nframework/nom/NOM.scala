package com.nframework.nom

import scala.collection.mutable._

object ENOMType extends Enumeration {
  val OBJECT = Value(1)
  val INTERACTION = Value
}

final class NOM {
  // properties
  var name = ""
  var note = ""
  var msgID = 0
  var instanceID = 0
  var ownwer = ""
  var nomType = ENOMType.OBJECT
  var alignment = false
  var omt = false
  var length = 0
  var pathList = new ListBuffer[String]
  
  // fields
  private var fieldList = ListBuffer.empty[NField]
  private var fieldMap = HashMap.empty[String, NField]
  private var dataTypeMap = HashMap.empty[String, NDataType]
  private var maxTypeLength: Short = 0
  
  // normal methods
  def addField(field: NField) {
    field.dataType.setNOM(this)
    fieldList += field
    fieldMap += (field.name -> field)
  }
  
  def getField(name: String) : NField = {
    fieldMap.get(name) match {
      case Some(f) => f
      case None => null
    }
  }
  
  def setValue(path: String, value: NValueType) : Boolean = {
    setValue(path, value, 0)
  }
  
  def setValue(path: String, value: NValueType, n: Int) : Boolean = {
    var success = false
    val dataType = getDataTypeObject(path)
    
    if(dataType != null && dataType.isLeaf()) {
      dataType.setValue(value, n)
      success = true
    } else {
      val builder = new StringBuilder
      builder.append("(NOM::setValue) ")
      builder.append(name)
      builder.append("::")
      builder.append(path)
      builder.append(" is invalid field!")
      
      throw new NInvalidFieldException(builder.toString())
    }
    
    success
  }
  
  def getValue(path: String) : NValueType = {
    getValue(path, 0)
  }
  
  def getValue(path: String, n: Int) : NValueType = {
    var value : NValueType = null
    val dataType = getDataTypeObject(path)
    
    if(dataType != null && dataType.isLeaf()) {
      value = dataType.getValue(n)
    } else {
      val builder = new StringBuilder
      builder.append("(NOM::getValue) ")
      builder.append(name)
      builder.append("::")
      builder.append(path)
      builder.append(" is invalid field!")
      
      throw new NInvalidFieldException(builder.toString())
    }
    
    value
  }
  
  def getDataTypeObject(path: String) : NDataType = {
    dataTypeMap.get(path) match {
      case Some(dt) => dt
      case None => null
    }
  }
  
  def addDataTypeObjectWithPath(path: String, dataType: NDataType) {
    if(path.length() != 0) {
      dataTypeMap += (path -> dataType)
      
      if(pathList.filter(_ == path).length == 0)
        pathList += path
    }
  }
  
  def getLength() : Int = {
    var nextTypeLength : Short = 0
    
    length = 0
    
    var field: NField = null
    var nextField: NField = null
    var offset = 0
    
    for(i <- (0 to fieldList.length-1)) {
      field = fieldList(i)
      
      if(alignment) {
        if(i == fieldList.length - 1)
          nextField = null
        else
          nextField = fieldList(i + 1)
          
        if(nextField != null)
          nextTypeLength = nextField.getMaxTypeLength()
        else
          nextTypeLength = 0
          
        val fieldLength = field.getLength(alignment, nextTypeLength, offset)
        length += fieldLength
        offset += fieldLength
      }
    }
    
    length
  }
  
  def setLength(length: Int, path: String) {
    this.length = length
  }
  
  def setMaxTypeLength(maxTypeLength: Short) {
    this.maxTypeLength = maxTypeLength
    
    fieldList.foreach(_.alignmentLength = maxTypeLength)
  }
  
  def compositeDataTypeMap(dataType: NDataType, nom: NOM, parentPath: String) {
    val builder = new StringBuilder
    
    if(dataType.isLeaf()) {
      // leaf 노드인 경우 parent path에 .과 name맡 붙여서 path를 완성한다.
      builder.append(parentPath)
      
      if(parentPath.length() != 0)
        builder.append(".")
      
      builder.append(dataType.name)
      
      dataType.path = builder.toString()
      nom.addDataTypeObjectWithPath(dataType.path, dataType)
      
    } else {
      var tempParentPath = ""
      var newParentPath = ""
      var i = 0
      
      if(parentPath.length() != 0) {
        builder.append(parentPath)
        builder.append(".")
      }
      builder.append(dataType.name)
      
      dataType.path = builder.toString()
      tempParentPath = dataType.path
      nom.addDataTypeObjectWithPath(dataType.path, dataType)
      
      for(idx <- (0 to dataType.size-1)) {
        if(dataType.size > 1) {
          val indexBuilder = new StringBuilder
          indexBuilder.append(tempParentPath)
          indexBuilder.append("[")
          indexBuilder.append(idx)
          indexBuilder.append("]")
          
          newParentPath = indexBuilder.toString()
        } else {
          newParentPath = tempParentPath
        }
        
        var child = dataType.getChild(idx, i)
        
        while(child != null) {
          compositeDataTypeMap(child, nom, newParentPath)
          i += 1
          child = dataType.getChild(idx, i)
        } // while
        
        i = 0
      } // for
    } // else
  }
  
  def addDataTypeMapElement(parent: String, dataType: NDataType) {
    val builder = new StringBuilder
    var path = ""
    
    if(dataType.isLeaf()) {
      builder.append(parent)
      if(parent.length() != 0)
        builder.append(".")
      builder.append(dataType.name)
      
      path = builder.toString()
      dataTypeMap += (path -> dataType)
      
      if(pathList.filter(_ == path).length == 0)
        pathList += path
        
    } else {
      for(i <- (0 to dataType.getChildCount()-1)) {
        val child = dataType.getChild(0, i)
        
        // recursive call
        builder.append(parent)
        if(parent.length() != 0)
          builder.append(".")
        builder.append(dataType.name)
        
        if(dataType.size > 1) {
          builder.append("[")
          builder.append(i)
          builder.append("]")
        }
        
        addDataTypeMapElement(builder.toString(), child)
      }
    }
  }
  
  def deleteDataTypeMapElement(path: String) {
    dataTypeMap.get(path) match {
      case Some(dt) => {
        // 하위 path도 의미가 없으므로 삭제한다.
        val listForRemove = ListBuffer.empty[String]
        
        // 대상을 찾아서 list에 add
        dataTypeMap.filter(_._1 == path).map(_._1).foreach(listForRemove += _)
        
        // 삭제
        listForRemove.foreach( (s: String) => {
          dataTypeMap -= s
          pathList -= s
        })        
      }
      
      case None => // do nothing
    }
  }
  
  def setOMT(b: Boolean) {
    omt = b
    
    fieldList.foreach(_.setOMT(b))
  }
  
  def getClone() : NOM = {
    val nom = new NOM
    
    nom.name = name
    nom.note = note
    nom.alignment = alignment
    nom.msgID = msgID
    nom.omt = omt
    
    var prevField: NField = null
    
    fieldList.foreach( (f: NField) => {
      val newField = new NField(f)
      newField.alignment = alignment
      
      if(prevField != null)
        prevField.nextField = newField
        
      prevField = newField
      
      nom.addField(newField)
      
      compositeDataTypeMap(newField.dataType, nom, "")
    })
    
    nom
  }
  
  def serialize() : (Array[Byte], Int) = {
    val totalLength = getLength()
    var data: Array[Byte] = null
    
    var field: NField = null
    var nextField: NField = null
    var fieldLength = 0
    var lengthCount = 0
    var offset = 0
    var nextTypeLength: Short = 0
    
    if(totalLength != 0) {
      data = new ArrayBuffer[Byte](totalLength + 1).toArray
            
      for(i <- (0 to fieldList.length-1)) {
        field = fieldList(i)
        
        if(i != fieldList.length - 1)
          nextField = fieldList(i + 1)
        else
          nextField = null
          
        if(nextField != null)
          nextTypeLength = nextField.getMaxTypeLength()
        else
          nextTypeLength = 0
          
        val info = field.serialize(data, fieldLength, offset, alignment, nextTypeLength)
        fieldLength = info._1
        offset = info._2
        lengthCount += fieldLength
      }
      
      if(lengthCount != totalLength) {
        val builder = new StringBuilder
        builder.append("[NOM::serialize] NOM의 length와 serialize의 length가 맞지 않음 : ")
        builder.append(lengthCount)
        builder.append("/")
        builder.append(totalLength)
        
        println(builder.toString())
      }
      
      //length = lengthCount
    }
    
    (data, lengthCount)
  }
  
  def deserialize(data: Array[Byte], length: Int) : Boolean = {
    var totalLength = 0
    
    var field: NField = null
    var nextField: NField = null
    var fieldLength = 0
    var lengthCount = 0
    var offset = 0
    var nextTypeLength: Short = 0
    
    for(i <- (0 to fieldList.length-1)) {
      field = fieldList(i)
      
      if(i != fieldList.length - 1)
        nextField = fieldList(i + 1)
      else
        nextField = null
        
      if(nextField != null)
        nextTypeLength = nextField.getMaxTypeLength()
      else
        nextTypeLength = 0
        
      val info = field.deserialize(data, fieldLength, offset, alignment, nextTypeLength)
      fieldLength = info._2
      offset = info._3
      
      if(totalLength + fieldLength > length) {
        println("[NOM::deserialize] deserialize된 길이가 주어진 buffer size를 초과함! deserialize 실패!");
        return false
      }
      
      lengthCount += fieldLength
      totalLength += fieldLength
      offset += fieldLength
    }
    
    true
  }
  
  def copyTo(to: NOM) : Boolean = {
    var success = true
    
    if(name == to.name && msgID == to.msgID && fieldList.length == to.fieldList.length) {
      for(i <- (0 to fieldList.length)) {
        val fieldFrom = fieldList(i)
        val fieldTo = to.fieldList(i)
        
        if(fieldFrom != null && fieldTo != null) {
          if(!fieldFrom.copyTo(fieldTo)) {
            success = false            
          }
        }
      }
      
      to.length = this.length
    } else {
      success = false
    }
    
    success
  }
}