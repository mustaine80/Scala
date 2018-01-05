package com.nframework.nom

import scala.util.parsing.json._
import java.io._

trait Parser {
  def parse() : Boolean
  def parse(path: String) : Boolean
  
  var path = ""
}

case class BasicType(name: String, length: Int, endian: String, typeName: String, dataType: EDataType.Value)
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

case class BasicTypeInfo(fieldName: String, path: String, indicator: Int, fixedLength: Int, typeName: String, size: Int)
case class EnumTypeInfo(fieldName: String, path: String, indicator: Int)
case class ComplexTypeInfo(fieldName: String, path: String, indicator: Int, fixedLength: Int, typeName: String, size: Int, msg: NMessage)

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
  
  val primitiveTypeMap = collection.mutable.HashMap.empty[String, EDataType.Value]
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
    clearMessageList()
    
    primitiveTypeMap += ("Bool" -> EDataType.BOOL)
    primitiveTypeMap += ("Byte" -> EDataType.BYTE)
    primitiveTypeMap += ("Char" -> EDataType.CHAR)
    primitiveTypeMap += ("Short" -> EDataType.SHORT)
    primitiveTypeMap += ("Integer" -> EDataType.INTEGER)
    primitiveTypeMap += ("Float" -> EDataType.FLOAT)
    primitiveTypeMap += ("Double" -> EDataType.DOUBLE)
    primitiveTypeMap += ("String" -> EDataType.STRING)
    primitiveTypeMap += ("Variable" -> EDataType.VARIABLE)
    primitiveTypeMap += ("FixedString" -> EDataType.FIXED_STRING)
    primitiveTypeMap += ("FixedDatum" -> EDataType.FIXED_DATUM)
    
    val fileutf8 = new BufferedReader(new InputStreamReader(new FileInputStream(new File(path)), "UTF-8") )
    
    var line = ""
    var jsonstrutf8 = ""
    
    line = fileutf8.readLine
    
    while(line != null) {
      jsonstrutf8 += line
      line = fileutf8.readLine
    }
    
    val json = JSON.parseFull(jsonstrutf8)
    
    val result : Boolean = if(json != None)
    {  
      var success = true
      
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
    
      try {
        composeBasicTypeList(basicTypeMap)
        composeEnumTypeList(enumTypeMap)
        composeComplexTypeList(complexTypeMap)
        composeObjectList(objectMap)
        composeInteractionList(interactionMap)
            
        checkBasicTypeList()
        checkEnumTypeList()
        checkComplexTypeList()
        checkObjectList()
        checkInteractionList()
      } catch {
        case e: NNOMFileParsingFailedException => println(e.toString())
        success = false 
      }
      
      success
    } else { false }
    
    result
  }

  override def parse(path: String) : Boolean = {
    this.path = path
    parse()
  }
  
  def composeBasicTypeList(m: Map[String, Map[String, String]]) {
    m.map( ( e: (String, Map[String, String]) ) => BasicType(e._1, e._2("length").toInt, e._2("endian"), e._2("type"), primitiveTypeMap(e._2("type")) ) ).foreach( basicTypeList += _ )
    
    // 타입 이름 중복 체크 => 처음 가져올 때 map에서 중복이 걸러지므로 아래 코드는 의미가 없어a
    basicTypeList.groupBy(_.name).collect { case (x, ys) if ys.lengthCompare(1) > 0 => {
          val builder = new StringBuilder
          builder.append("NOM parse failed: duplicated basic type (")
          builder.append(x)
          builder.append(")")
          
          throw new NNOMFileParsingFailedException(builder.toString())
        }
      }
    
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
  
  private def checkBasicTypeList() {
    basicTypeList.foreach( (bt: BasicType) => {
      primitiveTypeMap.get(bt.typeName) match {
        case Some(pt) =>
        case None => {
          val builder = new StringBuilder
          builder.append("NOM parse failed: ")
          builder.append(bt.typeName)
          builder.append(" is not a correct type name.");
          throw new NNOMFileParsingFailedException(builder.toString())
        }
      }
    })
  }
  
  private def checkEnumTypeList() {
    enumTypeList.foreach( (et: EnumType) => {
      basicTypeMap.get(et.name) match {
        case Some(bt) => {
          val builder = new StringBuilder
          builder.append("NOM parse failed : Enum type ")
          builder.append(et.name)
          builder.append(" is duplicated with another basic data type.");
          throw new NNOMFileParsingFailedException(builder.toString())
        }
        case None => {
          complexTypeMap.get(et.name) match {
            case Some(ct) => {
              val builder = new StringBuilder
              builder.append("NOM parse failed : Enum type ")
              builder.append(et.name)
              builder.append(" is duplicated with another complex data type.");
              throw new NNOMFileParsingFailedException(builder.toString())
            }
            case None =>
          }
        } // case None
      }
      
    }) // foreach
  }
  
  private def checkComplexTypeList() {
    complexTypeList.foreach( (ct: ComplexType) => {
      basicTypeMap.get(ct.name) match {
        case Some(bt) => {          
          val builder = new StringBuilder
          builder.append("NOM parse failed : Complex type ")
          builder.append(ct.name)
          builder.append(" is duplicated with another basic data type.");
          throw new NNOMFileParsingFailedException(builder.toString())
        }
        case None => {
          enumTypeMap.get(ct.name) match {
            case Some(et) => {
              val builder = new StringBuilder
              builder.append("NOM parse failed : Complex type ")
              builder.append(ct.name)
              builder.append(" is duplicated with another enum data type.");
              throw new NNOMFileParsingFailedException(builder.toString())
            }
            case None =>
          }
        } // case None
      }
      
    }) // foreach
  }
  
  private def checkObjectList() {
    objectList.foreach( (o: Object) => {
      o.attributeList.foreach( (f: Attribute) => {
        var foundBasicType = false
        var foundEnumType = false
        var foundComplexType = false
        
        basicTypeMap.get(f.dataType) match {
          case Some(bt) => foundBasicType = true
          case None =>
        }
        
        enumTypeMap.get(f.dataType) match {
          case Some(et) => foundEnumType = true
          case None =>
        }
        
        complexTypeMap.get(f.dataType) match {
          case Some(ct) => foundComplexType = true
          case None =>
        }
        
        if( !(foundBasicType || foundEnumType || foundComplexType) ) {
          val builder = new StringBuilder
          builder.append("NOM parse failed : Object ")
          builder.append(o.name)
          builder.append("'s attribute ")
          builder.append(f.name)
          builder.append(" has an invalied data type (")
          builder.append(f.dataType)
          builder.append(")")
          throw new NNOMFileParsingFailedException(builder.toString())
        }
        
      })
    })
  }
  
  private def checkInteractionList() {
    interactionList.foreach( (i: Interaction) => {
      i.parameterList.foreach( (f: Parameter) => {
        var foundBasicType = false
        var foundEnumType = false
        var foundComplexType = false
        
        basicTypeMap.get(f.dataType) match {
          case Some(bt) => foundBasicType = true
          case None =>
        }
        
        enumTypeMap.get(f.dataType) match {
          case Some(et) => foundEnumType = true
          case None =>
        }
        
        complexTypeMap.get(f.dataType) match {
          case Some(ct) => foundComplexType = true
          case None =>
        }
        
        if( !(foundBasicType || foundEnumType || foundComplexType) ) {
          val builder = new StringBuilder
          builder.append("NOM parse failed : Interaction ")
          builder.append(i.name)
          builder.append("'s parameter ")
          builder.append(f.name)
          builder.append(" has an invalied data type (")
          builder.append(f.dataType)
          builder.append(")")
          throw new NNOMFileParsingFailedException(builder.toString())
        }
        
      })
    })
  }
  
  def createPrimitiveTypeObject(basic: BasicType, info: BasicTypeInfo) : NPrimitiveType = {
    val pt = new NPrimitiveType
    var value: NValueType = null
    
    basic.dataType match {
      case EDataType.BOOL => value = new NBool
      case EDataType.BYTE => value = new NByte
      case EDataType.CHAR => {
        value = new NChar
        if(basic.length != 0)
          value.typeLength = basic.length
      }
      case EDataType.SHORT => value = new NShort
      case EDataType.INTEGER => {
        value = new NInteger
        value.length = basic.length
        if(basic.length != 0)
          value.typeLength = basic.length
      }
      case EDataType.FLOAT => value = new NFloat
      case EDataType.DOUBLE => value = new NDouble
      case EDataType.STRING => {
        value = new NString
        value.typeLength = if(basic.length != 0) basic.length else 0
      }
      case EDataType.VARIABLE => value = new NVariable
      case EDataType.FIXED_STRING => {
        value = if(basic.length != 0) new NFixedString(basic.length.toShort, info.fixedLength) else new NFixedString(info.fixedLength)        
      }
      case EDataType.FIXED_DATUM => value = new NFixedDatum(info.fixedLength)
    }
    
    value.bigEndian = basic.endian match {
      case "big" => true
      case "little" => false
      case _ => false
    }
    value.signed = true
    value.path = path
    value.length = if(basic.dataType == EDataType.STRING || basic.dataType == EDataType.VARIABLE) 4 else basic.length
        
    pt.name = info.fieldName
    pt.path = info.path
    pt.indicator = info.indicator
    pt.typeName = info.typeName
    
    if(info.size > 1)
      pt.setSize(info.size)
    
    pt.setValueObject(value)
    
    pt
  }
  
  def createEnumTypeObject(enum: EnumType, info: EnumTypeInfo) : NEnumType = {
    val et = new NEnumType
    
    enum.enumList.foreach( (e: Enumerator) => et.addEnumerator(e.name, e.value) )
    
    et.name = info.fieldName
    et.path = info.path
    et.indicator = info.indicator
    et.length = enum.length
    
    et
  }
  
  def createComplexTypeObject(complex: ComplexType, info: ComplexTypeInfo) : NComplexType = {
    val ct = new NComplexType
    
    ct.name = info.fieldName
    ct.path = info.path
    ct.indicator = info.indicator
    ct.typeName = info.typeName
    
    var childTypeName = ""
    
    complex.fieldList.foreach( (f: Field) => {
      
    })
    
    ct
  }
  
  def getMessageList() : List[NMessage] = msgList.toList
  
  def getObjectList() : List[NMessage] = msgList.filter(_.nomType == ENOMType.OBJECT).toList
  
  def getInteractionList() : List[NMessage] = msgList.filter(_.nomType == ENOMType.INTERACTION).toList
  
  def getMessageMap() : Map[String, NMessage] = msgMap.toMap
  
  def getMessageIDMap() : Map[Int, NMessage] = msgIDMap.toMap
  
  def getPrimitiveTypeMap() : Map[String, EDataType.Value] = primitiveTypeMap.toMap
  
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
  
  def asUnsigned(v: Byte) = if(v < 0) 256 + v else v
  val s = "안녕1"
  val arr = s.getBytes("UTF-16LE").map(asUnsigned(_))
  
  val s2 = new String(s.getBytes("UTF-16LE"), "UTF-16LE")
  println(s2)
  
  println(arr.mkString(", "))
}