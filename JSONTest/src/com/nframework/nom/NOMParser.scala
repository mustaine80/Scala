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
  
  override def parse() : Boolean = {
    val fileutf8 = new BufferedReader(new InputStreamReader(new FileInputStream(new File(path)), "UTF-8") )
    
    var line = ""
    var jsonstrutf8 = ""
    
    line = fileutf8.readLine
    
    while(line != null) {
      jsonstrutf8 += line
      line = fileutf8.readLine
    }
    
    jsonstrutf8 = jsonstrutf8.drop(1)  // UTF-8로 인코딩된 파일은 string 앞에 -1이 붙으므로 첫 글자를 잘라야 함


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
}

object JSONTest extends App {
  
  
  println("JSON Test!")

  NOMParser.parse("test.json")
  
}
