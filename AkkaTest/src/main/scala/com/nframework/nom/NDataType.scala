package com.nframework.nom

sealed abstract class NOM


case class NMessage(name: String, data: Array[Byte]) extends NOM


/** Manager 간 NOM message 를 보낼 때 아래의 메소드를 사용하여 wrapping 해야 한다.
  * loadMessage 를 이용하여 객체 정보를 NMessage 로 변환한 후 manager actor 에게 전송한다.
  * manager actor 에 수신된 메시지는 unloadMessage 를 이용하여 객체 필드 업데이트 후 반환한다.
  */
//case object NMessage {
//  def loadMessage(msgName: String, data: AnyRef): NMessage = {
//    val bytes = getByteArrayTemplate(msgName)(data)
//    NMessage(msgName, bytes)  /// dummy Impl
//  }
//
//
//
//  private def getByteArrayTemplate(name: String): Function1[AnyRef, Array[Byte]] = ???   /// "field1" : serialize
//  private def getNOMTemplate(name: String): Function1[Array[Byte], AnyRef] = ???   /// "field1" : NDouble
//}





//case class DummyHead(objName: String) extends NOM(objName) {
//
//}
//
//case class DummyNOM(objName: String, value: NChar) extends NOM(objName) {
//
//}


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
  def getLength(alignment: Boolean, nextTypeLength: Short, offset: Int)
  def setAlignmentLength(length: Short)
  def getTypeLength() : Short
  def getMaxTypeLEngth() : Short
  def copyTo(to: NDataType) : Boolean
  def setOMT(b: Boolean)

  def serialize(data: Array[Byte], length: Int, offset: Int, alignment: Boolean, nextTypeLength: Short)
  def deserialize(data: Array[Byte], length: Int, offset: Boolean, nextTypeLength: Short) : Boolean
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

  def serializeIndicator(data: Array[Byte], length: Int, offset: Int, alignment: Boolean) {

  }

  def deserializeIndicator(data: Array[Byte], length: Int, fieldSize: Int, offset: Int, alignment: Boolean) = {


    false
  }
}
