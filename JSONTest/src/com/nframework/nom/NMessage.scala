package com.nframework.nom

import scala.collection.mutable._

object ESharing extends Enumeration {
  val NEITHER = Value(1)
  val PUBLISH = Value
  val SUBSCRIBE = Value
  val PUBLISHSUBSCRIBE = Value
}

final class NMessage {
  // properties
  var name = ""
  var note = ""
  var msgID = 0
  var instanceID = 0
  var ownwer = ""
  var nomType = ENOMType.OBJECT
  var sharing = ESharing.NEITHER
  var alignment = false
  var omt = false
  var length = 0
  var pathList = new ListBuffer[String]
  
  // fields
  private var fieldList = ListBuffer.empty[NField]
  private var fieldMap = HashMap.empty[String, NField]
  private var dataTypeMap = HashMap.empty[String, NDataType]
  
  // normal methods
  def addField(field: NField) {
    fieldList += field
    fieldMap += (field.name -> field)
  }
  
  def getField(name: String) : NField = {
    fieldMap.get(name) match {
      case Some(v) => v
      case None => null
    }    
  }
  
  def addDataTypeObjectWithPath(path: String, dataType: NDataType) {
    dataTypeMap += (path -> dataType)
  }
  
  def getFieldList() : List[NField] = fieldList.toList
  
  def createNOMInstance() : NOM = {
    val nom  = new NOM
    
    nom.name = name
    nom.note = note
    nom.msgID = msgID
    nom.nomType = nomType
    nom.alignment = alignment
    nom.omt = omt
    
    var maxTypeLength : Short = 0
    var prevField: NField = null
    
    fieldList.foreach((f: NField) => {
      val newField = new NField(f)
      newField.alignment = alignment
      if(prevField != null)
        prevField.nextField = f
      prevField = newField
      
      nom.addField(newField)
      
      if(maxTypeLength < newField.getMaxTypeLength())
        maxTypeLength = newField.getMaxTypeLength()
        
      compositeDataTypeMap(newField.dataType, nom, "");
    })
    
    nom.setMaxTypeLength(maxTypeLength)
        
    nom
  }
  
  def compositeDataTypeMap(dataType: NDataType, nom: NOM, parentPath: String) {
    var builder = new StringBuilder
    
    if(dataType.isLeaf()) {
      // Leaf 노드인 경우 parent path에 .과 name맡 붙여서 path를 완성한다.
      builder.append(parentPath)
      
      if(parentPath.length() != 0) {
        builder.append(".")
      }
      
      builder.append(dataType.name)
     
      dataType.path = builder.toString()
      nom.addDataTypeObjectWithPath(dataType.path, dataType)     
    } else {
      var tempParentPath = ""
      var newParentPath = ""
      // non leaf 노드인 경우 (complex type) 배열 처리를 하여 path를 완성한다.
      if(parentPath.length() != 0) {
        builder.append(parentPath)
        builder.append(".")
      }
      
      builder.append(dataType.name)
      
      dataType.path = builder.toString()
      tempParentPath = dataType.path
      nom.addDataTypeObjectWithPath(dataType.path, dataType)
      
      var i = 0
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
}