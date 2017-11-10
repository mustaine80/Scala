package com.nframework.nom


trait NValueType {
  var path = ""
  var nom: NOM = null
  
  var bigEndian = false
  var signed = true
  var length = 0
  var variable = false
  var typeLength = 0
  
  def setValue(valueType: NValueType) : Boolean
  def getClone() : NValueType
  def copyTo(to: NValueType)
  def serialize(length: Int) : Array[Byte]
  def deserialize(data: Array[Byte], offset: Int, length: Int)
}
