package com.nframework.nom

import scala.util.parsing.json._
import java.io._

trait Parser {
  def parse() : Boolean
  def parse(path: String) : Boolean
  
  var path = ""
}

case class BasicType(name: String, length: Int, endian: String, dataType: String)
case class Enumerator(name: String, value: Int)
case class EnumType(name: String, length: Int) {  
  val enumList: collection.mutable.ListBuffer[Enumerator] = new collection.mutable.ListBuffer
}

case class Field(name: String, size: Int, dataType: String)
case class ComplexType(name: String) {  
  val fieldList: collection.mutable.ListBuffer[Field] = new collection.mutable.ListBuffer
}

case class Attribute(name: String, size: Int, dataType: String, fixedLength: Int, indicator: Int)
case class Object(name: String, sharing: String, alignment: Boolean) {  
  val attributeList: collection.mutable.ListBuffer[Attribute] = new collection.mutable.ListBuffer
}

case class Parameter(name: String, size: Int, dataType: String, fixedLength: Int, indicator: Int)
case class Interaction(name: String, sharing: String, alignment: Boolean) {
  val parameterList: collection.mutable.ListBuffer[Parameter] = new collection.mutable.ListBuffer
}

object NOMParser extends Parser {
  val basicTypeList: collection.mutable.ListBuffer[BasicType] = new collection.mutable.ListBuffer
  val enumTypeList: collection.mutable.ListBuffer[EnumType] = new collection.mutable.ListBuffer
  val complexTypeList: collection.mutable.ListBuffer[ComplexType] = new collection.mutable.ListBuffer
  val objectList: collection.mutable.ListBuffer[Object] = new collection.mutable.ListBuffer
  val interactionList: collection.mutable.ListBuffer[Interaction] = new collection.mutable.ListBuffer
  
  var basicTypeMap: Map[String, BasicType] = null
  var enumTypeMap: Map[String, EnumType] = null
  var complexTypeMap: Map[String, ComplexType] = null
  var objectMap: Map[String, Object] = null
  var interactionMap: Map[String, Interaction] = null
  
  val primitiveTypeMap = collection.mutable.HashMap.empty[String, Int]
  val msgMap = collection.mutable.HashMap.empty[String, NMessage]
  val msgIDMap = collection.mutable.HashMap.empty[Int, NMessage]
  val msgList = collection.mutable.ListBuffer.empty[NMessage]
  
  private def clearMessageList() {
    basicTypeList.clear()
    enumTypeList.clear()
    complexTypeList.clear()
    objectList.clear()
    interactionList.clear()
    
    basicTypeMap = null
    enumTypeMap = null
    complexTypeMap = null
    objectMap = null
    interactionMap = null
    
    primitiveTypeMap.clear()
    msgMap.clear()
    msgIDMap.clear()
  }
  
  override def parse() : Boolean = {
    val fileutf8 = new BufferedReader(new InputStreamReader(new FileInputStream(new File(path)), "UTF-8") )
    
    var line = ""
    var jsonstrutf8 = ""
    
    line = fileutf8.readLine
    
    while(line != null) {
      jsonstrutf8 += line
      line = fileutf8.readLine
    }
    
    clearMessageList()
    
    primitiveTypeMap += ("Bool" -> EDataType.BOOL.asInstanceOf[Int])
    primitiveTypeMap += ("Byte" -> EDataType.BYTE.asInstanceOf[Int])
    primitiveTypeMap += ("Char" -> EDataType.CHAR.asInstanceOf[Int])
    primitiveTypeMap += ("Short" -> EDataType.SHORT.asInstanceOf[Int])
    primitiveTypeMap += ("Integer" -> EDataType.INTEGER.asInstanceOf[Int])
    primitiveTypeMap += ("Float" -> EDataType.FLOAT.asInstanceOf[Int])
    primitiveTypeMap += ("Double" -> EDataType.DOUBLE.asInstanceOf[Int])
    primitiveTypeMap += ("String" -> EDataType.STRING.asInstanceOf[Int])
    primitiveTypeMap += ("Variable" -> EDataType.VARIABLE.asInstanceOf[Int])
    primitiveTypeMap += ("FixedString" -> EDataType.FIXED_STRING.asInstanceOf[Int])
    primitiveTypeMap += ("FixedDatum" -> EDataType.FIXED_DATUM.asInstanceOf[Int])


    val json = JSON.parseFull(jsonstrutf8)
    
    val result : Boolean = if(json != None)
    {    
      val root = json.get.asInstanceOf[Map[String, Any]]
      
      val basicTypeMap = root("BasicTypes").asInstanceOf[Map[String, Map[String, String]]]
      val basicTypeList = basicTypeMap.toList
      
      val enumTypeMap = root("EnumTypes").asInstanceOf[Map[String, Map[String, String]]]
      val enumTypeList = enumTypeMap.toList
      
      val complexTypeMap = root("ComplexTypes").asInstanceOf[Map[String, Map[String, Map[String, String]]]]
      val complexTypeList = complexTypeMap.toList
      
      
      val objectMap = root("Objects").asInstanceOf[Map[String, Map[String, Map[String, String]]]]
      val objectList = objectMap.toList
      
      val interactionMap = root("Interactions").asInstanceOf[Map[String, Map[String, Map[String, String]]]]
      val interactionList = interactionMap.toList
    
      /*
      println(basicTypeList)
      println(enumTypeList)
      println(complexTypeList)
      println(objectList)
      println(interactionList)
      */
      
      composeBasicTypeList(basicTypeMap)
      composeEnumTypeList(enumTypeMap)
      composeComplexTypeList(complexTypeMap)
      composeObjectList(objectMap)
      composeInteractionList(interactionMap)
      
      true
    } else { false }
    
    result
  }

  override def parse(path: String) : Boolean = {
    this.path = path
    parse()
  }
  
  def composeBasicTypeList(m: Map[String, Map[String, String]]) {
    m.map( ( e: (String, Map[String, String]) ) => BasicType(e._1, Integer.parseInt(e._2("length")), e._2("endian"), e._2("type")) ).foreach( basicTypeList += _ ) 
    
    println(basicTypeList)
    basicTypeMap = basicTypeList.map( (e) => (e.name, e) ).toMap
  }
  
  def composeEnumTypeList(m: Map[String, Map[String, String]]) {    
    m.map( (e: (String, Map[String, String]) ) => {
      val et = EnumType(e._1, Integer.parseInt(e._2("length")))
      e._2.filter(!_._1.equals("length")).foreach( (en: (String, String)) => et.enumList += Enumerator(en._1, Integer.parseInt(en._2)) )
      et
    }).foreach( enumTypeList += _ )
  
    enumTypeList.foreach( (e) => println(e.enumList) )
    enumTypeMap = enumTypeList.map( (e) => (e.name, e) ).toMap
  }
  
  def composeComplexTypeList(m: Map[String, Map[String, Map[String, String]]]) {
    m.map( (e: (String, Map[String, Map[String, String]])) => {
      val ct = ComplexType(e._1)
      e._2.foreach( (f : (String, Map[String, String])) => ct.fieldList += Field(f._1, Integer.parseInt(f._2("size")), f._2("type")) )
      ct
    }).foreach( complexTypeList += _ )
    
    complexTypeList.foreach( (e) => println(e.fieldList) )
    complexTypeMap = complexTypeList.map( (e) => (e.name, e) ).toMap
  }
  
  def composeObjectList(m: Map[String, Map[String, Map[String, String]]]) {
    m.map( (e: (String, Map[String, Map[String, String]])) => {
      val ob = Object(e._1, e._2("sharing").asInstanceOf[String], (e._2("alignment").asInstanceOf[String]=="true"))
      e._2.filter( (t: (String, Map[String, String]) ) => !(t._1.equals("sharing") || t._1.equals("alignment")) ).foreach( (f : (String, Map[String, String])) => { 
        ob.attributeList += Attribute(f._1, Integer.parseInt(f._2("size")), f._2("type"), Integer.parseInt(f._2("fixedLength")), Integer.parseInt(f._2("indicator")) ) 
            } )
      ob
    }).foreach( objectList += _ )
    
    objectList.foreach( (e) => println(e.attributeList) )
    objectMap = objectList.map( (e) => (e.name, e) ).toMap
  }
  
  def composeInteractionList(m: Map[String, Map[String, Map[String, String]]]) {
    m.map( (e: (String, Map[String, Map[String, String]])) => {
      val it = Interaction(e._1, e._2("sharing").asInstanceOf[String], (e._2("alignment").asInstanceOf[String]=="true"))
      e._2.filter( (t: (String, Map[String, String]) ) => !(t._1.equals("sharing") || t._1.equals("alignment")) ).foreach( (f : (String, Map[String, String])) => { 
        it.parameterList += Parameter(f._1, Integer.parseInt(f._2("size")), f._2("type"), Integer.parseInt(f._2("fixedLength")), Integer.parseInt(f._2("indicator")) ) 
            } )
      it
    }).foreach( interactionList += _ )
    
    interactionList.foreach( (e) => println(e.parameterList) )
    interactionMap = interactionList.map( (e) => (e.name, e) ).toMap
  }
  
  def createPrimitiveTypeObject(basic: BasicType) : NPrimitiveType = {
    var pt: NPrimitiveType = null
    var value: NValueType = null
    
    basic.dataType match {
      case "Bool" => value = new NBool
      case "Byte" => value = new NByte
      case "Char" => {
        value = new NChar
        if(basic.length != 0)
          value.typeLength = basic.length
      }
      case "Short" => value = new NShort
      case "Integer" => {
        value = new NInteger
        value.length = basic.length
        if(basic.length != 0)
          value.typeLength = basic.length
      }
      case "Float" => value = new NFloat
      case "Double" => value = new NDouble
      case "String" => {
        //value = new NString
      }
      case "Variable" =>
      case "FixedString" =>
      case "FixedDatum" =>      
    }
    
    value.bigEndian = basic.endian match {
      case "big" => true
      case "little" => false
      case _ => false
    }
    value.signed = true
    
    pt
  }
  
  def createEnumTypeObject(enum: EnumType) : NEnumType = {
    null
  }
  
  def createComplexTypeObject(complex: ComplexType) : NComplexType = {
    null
  }
  
  def getMessageList() : List[NMessage] = msgList.toList
  
  def getObjectList() : List[NMessage] = msgList.filter(_.nomType == ENOMType.OBJECT).toList
  
  def getInteractionList() : List[NMessage] = msgList.filter(_.nomType == ENOMType.INTERACTION).toList
  
  def getMessageMap() : Map[String, NMessage] = msgMap.toMap
  
  def getMessageIDMap() : Map[Int, NMessage] = msgIDMap.toMap
  
  def getPrimitiveTypeMap() : Map[String, Int] = primitiveTypeMap.toMap
  
  def getBasicTypeMap() : Map[String, BasicType] = basicTypeMap.toMap
  
  def getComplexTypeMap() : Map[String, ComplexType] = complexTypeMap.toMap
  
  def getEnumTypeMap() : Map[String, EnumType] = enumTypeMap.toMap
  
  def getMessageObject(name: String) = msgMap.getOrElse(name, null)
  
  def getMessageObject(id: Int) = msgIDMap.getOrElse(id, null)
  
  
}


object JSONTest2 extends App {
  
  
  println("JSON Test!")

  NOMParser.parse("test.json")
  
  // val a: Array[Byte] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  // println( NValueType.reverseBytes(a).mkString(", ") )
  // println( NValueType.reverseBytes(a, 4).mkString(", ") )
  // println( NValueType.reverseBytes(a, 2, 4).mkString(", ") )
  
  // import scala.math.BigInt
  // println( BigInt(1).toByteArray.mkString(", ") )
  // println( BigInt(0x12345678).toByteArray.length )  
  
  // java.nio.ByteBuffer를 사용한 encoding & decoding 테스트
  val bb = java.nio.ByteBuffer.allocate(8)
    
  bb.putInt(0x12345678)
  
  var buffer = bb.array()
  val rev = NValueType.reverseBytes(buffer)
  
  println(buffer.mkString(", "))
  println(rev.mkString(", "))
  
  println(java.nio.ByteBuffer.wrap(buffer).getInt)
  println(java.nio.ByteBuffer.wrap(rev).getInt)
  
  val d = new NDouble(1.234)
  val d2 = new NDouble(0.0)
  
  d2.deserialize(d.serialize()._1, 0)
  
  println(d2)
  
  // mutable hashmap에 대한 clone 테스트
//  val m1 = collection.mutable.HashMap( (1 -> "one"), (2->"two"), (3->"three"), (10->"ten") )
//  val m2 = m1.clone()
  
//  m2.foreach(println)
  
  // 이중 list에 대한 모든 값의 reduce 테스트
  //val ll = List( List(1, 2, 3, 4), List(5, 6), List(7), List(8, 9, 10) )
  //println( ll.map( (list: List[Int])=> { list.reduce(_ + _) }  ).reduce(_ + _) )  
}
