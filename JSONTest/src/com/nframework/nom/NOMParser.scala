package com.nframework.nom

import scala.util.parsing.json._
import java.io._
import scala.collection.immutable.ListMap

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

case class Field(name: String, size: Int, dataType: String, indicator: Int, fixedLength: Int)
case class ComplexType(name: String) {  
  val fieldList: collection.mutable.ListBuffer[Field] = new collection.mutable.ListBuffer
}

case class Attribute(name: String, size: Int, dataType: String, fixedLength: Int, indicator: Int)
case class NOMObject(nomType: ENOMType.Value, name: String, msgID: Int, sharing: String, alignment: Boolean) {  
  val attributeList: collection.mutable.ListBuffer[Attribute] = new collection.mutable.ListBuffer
}

case class BasicTypeInfo(fieldName: String, path: String, indicator: Int, fixedLength: Int, typeName: String, size: Int)
case class EnumTypeInfo(fieldName: String, path: String, indicator: Int)
case class ComplexTypeInfo(fieldName: String, path: String, indicator: Int, fixedLength: Int, typeName: String, size: Int)

class NOMParser extends Parser {
  private val basicTypeList: collection.mutable.ListBuffer[BasicType] = new collection.mutable.ListBuffer
  private val enumTypeList: collection.mutable.ListBuffer[EnumType] = new collection.mutable.ListBuffer
  private val complexTypeList: collection.mutable.ListBuffer[ComplexType] = new collection.mutable.ListBuffer
  private val objectList: collection.mutable.ListBuffer[NOMObject] = new collection.mutable.ListBuffer
  private val interactionList: collection.mutable.ListBuffer[NOMObject] = new collection.mutable.ListBuffer
  
  private var basicTypeMap: Map[String, BasicType] = null
  private var enumTypeMap: Map[String, EnumType] = null
  private var complexTypeMap: Map[String, ComplexType] = null
  private var objectMap: Map[String, NOMObject] = null
  private var interactionMap: Map[String, NOMObject] = null
  
  private val primitiveTypeMap = collection.mutable.HashMap.empty[String, EDataType.Value]
  private val msgMap = collection.mutable.HashMap.empty[String, NMessage]
  private val msgIDMap = collection.mutable.HashMap.empty[Int, NMessage]
  private val msgList = collection.mutable.ListBuffer.empty[NMessage]
  
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
      
      
      val objectMap = root("Objects").asInstanceOf[Map[String, ListMap[String, Map[String, String]]]]
      val objectList = objectMap.toList
      
      val interactionMap = root("Interactions").asInstanceOf[Map[String, Map[String, Map[String, String]]]]
      val interactionList = interactionMap.toList
    
      try {
        // 각 type 및 object/interaction에 대한 정보 parsing
        composeBasicTypeList(basicTypeMap)
        composeEnumTypeList(enumTypeMap)
        composeComplexTypeList(complexTypeMap)
        composeObjectList(objectMap)
        composeInteractionList(interactionMap)
            
        // 각 type 및 object/interaction에 대한 의존성 무결성 검증
        checkBasicTypeList()
        checkEnumTypeList()
        checkComplexTypeList()
        checkObjectList()
        checkInteractionList()
        
        // object/interaction에 대한 NMessage 객체 생성
        createMessageObjects()
        
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
  
  private def composeBasicTypeList(m: Map[String, Map[String, String]]) {
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
  
  private def composeEnumTypeList(m: Map[String, Map[String, String]]) {    
    m.map( (e: (String, Map[String, String]) ) => {
      val et = EnumType(e._1, Integer.parseInt(e._2("length")))
      e._2.filter(!_._1.equals("length")).foreach( (en: (String, String)) => et.enumList += Enumerator(en._1, Integer.parseInt(en._2)) )
      et
    }).foreach( enumTypeList += _ )
  
    enumTypeList.foreach( (e) => println(e.enumList) )
    enumTypeMap = enumTypeList.map( (e) => (e.name, e) ).toMap
  }
  
  private def composeComplexTypeList(m: Map[String, Map[String, Map[String, String]]]) {
    m.map( (e: (String, Map[String, Map[String, String]])) => {
      val ct = ComplexType(e._1)
      e._2.foreach( (f : (String, Map[String, String])) => ct.fieldList += Field(f._1, Integer.parseInt(f._2("size")), f._2("type"), 
          Integer.parseInt(f._2.getOrElse("indicatorLength", 0).toString()),
          Integer.parseInt(f._2.getOrElse("fixedLength", 0).toString()) )  // Field 
      ) // foreach
      ct
    }).foreach( complexTypeList += _ )
    
    complexTypeList.foreach( (e) => println(e.fieldList) )
    complexTypeMap = complexTypeList.map( (e) => (e.name, e) ).toMap
  }
  
  private def composeObjectList(m: Map[String, ListMap[String, Map[String, String]]]) {
    m.map( (e: (String, Map[String, Map[String, String]])) => {
      val ob = NOMObject(ENOMType.OBJECT, e._1, Integer.parseInt(e._2("msgID").asInstanceOf[String]), e._2("sharing").asInstanceOf[String], (e._2("alignment").asInstanceOf[String]=="true"))
      e._2.filter( (t: (String, Map[String, String]) ) => !(t._1.equals("msgID") || t._1.equals("sharing") || t._1.equals("alignment")) ).foreach( (f : (String, Map[String, String])) => { 
        ob.attributeList += Attribute(f._1, Integer.parseInt(f._2("size")), f._2("type"), Integer.parseInt(f._2("fixedLength")), Integer.parseInt(f._2("indicator")) ) 
            } )
      ob
    }).foreach( objectList += _ )
    
    objectList.foreach( (e) => println(e.attributeList) )
    objectMap = objectList.map( (e) => (e.name, e) ).toMap
  }
  
  private def composeInteractionList(m: Map[String, Map[String, Map[String, String]]]) {
    m.map( (e: (String, Map[String, Map[String, String]])) => {
      val it = NOMObject(ENOMType.INTERACTION, e._1, Integer.parseInt(e._2("msgID").asInstanceOf[String]), e._2("sharing").asInstanceOf[String], (e._2("alignment").asInstanceOf[String]=="true"))
      e._2.filter( (t: (String, Map[String, String]) ) => !(t._1.equals("msgID") || t._1.equals("sharing") || t._1.equals("alignment")) ).foreach( (f : (String, Map[String, String])) => { 
        it.attributeList += Attribute(f._1, Integer.parseInt(f._2("size")), f._2("type"), Integer.parseInt(f._2("fixedLength")), Integer.parseInt(f._2("indicator")) ) 
            } )
      it
    }).foreach( interactionList += _ )
    
    interactionList.foreach( (e) => println(e.attributeList) )
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
    objectList.foreach( (o: NOMObject) => {
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
    interactionList.foreach( (i: NOMObject) => {
      i.attributeList.foreach( (f: Attribute) => {
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
  
  private def createPrimitiveTypeObject(basic: BasicType, info: BasicTypeInfo) : NPrimitiveType = {
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
    
    println("PRIMITIVE TYPE: " + info.path + " (" + info.typeName + ")")
    
    pt
  }
  
  private def createEnumTypeObject(enum: EnumType, info: EnumTypeInfo) : NEnumType = {
    val et = new NEnumType
    
    enum.enumList.foreach( (e: Enumerator) => et.addEnumerator(e.name, e.value) )
    
    et.name = info.fieldName
    et.path = info.path
    et.indicator = info.indicator
    et.length = enum.length
    
    println("ENUM TYPE: " + info.path + " (" + enum.name + ")")
    
    et
  }
  
  private def createComplexTypeObject(complex: ComplexType, info: ComplexTypeInfo, msg: NMessage) : NComplexType = {
    val ct = new NComplexType
    
    ct.name = info.fieldName
    ct.path = info.path
    ct.indicator = info.indicator
    ct.typeName = info.typeName
    
    var childTypeName = ""
    
    complex.fieldList.foreach( (f: Field) => {
      childTypeName = f.dataType
      var size = f.size
      var indicator = f.indicator
      var fixedLength = f.fixedLength
      
      basicTypeMap.get(childTypeName) match {
        case Some(bt) => {
          // basic Type
          val pathBuilder = new StringBuilder
          
          pathBuilder.append(info.path)
          pathBuilder.append(".")
          pathBuilder.append(f.name)
          
          val basicInfo = BasicTypeInfo(f.name, pathBuilder.toString(), f.indicator, f.fixedLength, childTypeName, size)
          val childType = createPrimitiveTypeObject(bt, basicInfo)
          
          childType.typeName = childTypeName
          ct.addDataType(0, childType)
          
          if(msg != null)
            msg.addDataTypeObjectWithPath(basicInfo.path, childType)
        }
        case None => {
          enumTypeMap.get(childTypeName) match {
            case Some(et) => {
              // enum Type
              val pathBuilder = new StringBuilder
              
              pathBuilder.append(info.path)
              pathBuilder.append(".")
              pathBuilder.append(f.name)
              
              val enumInfo = EnumTypeInfo(f.name, pathBuilder.toString(), f.indicator)
              val childType = createEnumTypeObject(et, enumInfo)
              
              childType.typeName = childTypeName
              ct.addDataType(0, childType)
              
              if(msg != null)
                msg.addDataTypeObjectWithPath(enumInfo.path, childType)
            }
            case None => {
              complexTypeMap.get(childTypeName) match {
                case Some(cct) => {
                  // complex Type
                  val pathBuilder = new StringBuilder
              
                  pathBuilder.append(info.path)
                  pathBuilder.append(".")
                  pathBuilder.append(f.name)
                  
                  val complexInfo = ComplexTypeInfo(f.name, pathBuilder.toString(), f.indicator, info.fixedLength, info.typeName, info.size)
                  val childType = createComplexTypeObject(cct, complexInfo, msg)
                  
                  childType.typeName = childTypeName
                  ct.addDataType(0, childType)
                  
                  if(msg != null)
                    msg.addDataTypeObjectWithPath(complexInfo.path, childType)
                }
                case None => {
                  val builder = new StringBuilder
                  builder.append("NOM parse failed : ComplexType ")
                  builder.append(complex.name)
                  builder.append("'s field ")
                  builder.append(f.name)
                  builder.append(" has an invalied data type (")
                  builder.append(f.dataType)
                  builder.append(")")
                  throw new NNOMFileParsingFailedException(builder.toString())
                }
              }
            }
          }
        }
      }
    })
    
    if(info.size > 1)
      ct.setSize(info.size)
    
    println("COMPLEX TYPE: " + info.path + " (" + info.typeName + ")")  
      
    ct
  }
  
  private def createMessageObjects() {    
    val allMsgList = objectList ++ interactionList 
        
    allMsgList.foreach( (o: NOMObject) => {
      val msg = new NMessage()
      
      msg.name = o.name
      msg.msgID = o.msgID
      msg.nomType = o.nomType
      
      // msg ID 음수 검사
      if(msg.msgID < 0) {
        val builder = new StringBuilder
        builder.append("NOM parse failed: NOM name ")
        builder.append(msg.name)
        builder.append(" has a negative id number (")
        builder.append(msg.msgID)
        builder.append(")")
        throw new NNOMFileParsingFailedException(builder.toString())
      }
      
      // msg ID 검사
      if(msgIDMap.contains(msg.msgID)) {
        val builder = new StringBuilder
        builder.append("NOM parse failed: NOM id ")
        builder.append(msg.msgID)
        builder.append(" is duplicated (")
        builder.append(msg.name)
        builder.append(")")
        throw new NNOMFileParsingFailedException(builder.toString())
      }
      
      // sharing 검사
      msg.sharing = o.sharing match {
        case "Neither" => ESharing.NEITHER
        case "Publish" => ESharing.PUBLISH
        case "Subscribe" => ESharing.SUBSCRIBE
        case "PublishSubscribe" => ESharing.PUBLISHSUBSCRIBE
        case _ => {
          val builder = new StringBuilder
          builder.append("NOM parse failed: NOM name ")
          builder.append(msg.name)
          builder.append(" has an invalid sharing (")
          builder.append(o.sharing)
          builder.append(")")
          throw new NNOMFileParsingFailedException(builder.toString())
          ESharing.NEITHER
        }
      }
      
      o.attributeList.foreach( (attr: Attribute) => {
        val field = new NField
        
        field.name = attr.name
        field.size = attr.size
        field.indicator = attr.indicator
        field.fixedLength = attr.fixedLength
        field.typeName = attr.dataType
        
        // data type object for the field
        val dataType = attr.dataType
        
        basicTypeMap.get(dataType) match {
          case Some(bt) => {
            val info = BasicTypeInfo(field.name, field.name, field.indicator, field.fixedLength, field.typeName, field.size)
            val primitiveType = createPrimitiveTypeObject(bt, info)
            
            field.dataType = primitiveType
            msg.addDataTypeObjectWithPath(info.path, primitiveType)
            //println("Primitive: " + msg.name + "::" + field.dataType.path + "(" + field.dataType.typeName + ")")
          }
          case None => {
            enumTypeMap.get(dataType) match {
              case Some(et) => {
                val info = EnumTypeInfo(field.name, field.name, field.indicator)
                val enumType = createEnumTypeObject(et, info)
                
                field.dataType = enumType
                msg.addDataTypeObjectWithPath(info.path, enumType)
                //println("Enum: " + msg.name + "::" + field.dataType.path + "(" + field.dataType.typeName + ")")
              }
              case None => {
                complexTypeMap.get(dataType) match {
                  case Some(ct) => {
                    val info = ComplexTypeInfo(field.name, field.name, field.indicator, field.fixedLength, field.typeName, field.size)
                    val complexType = createComplexTypeObject(ct, info, msg)
                    
                    field.dataType = complexType
                    //println("Complex: " + msg.name + "::" + field.dataType.path + "(" + field.dataType.typeName + ")")
                  }
                  case None => // never reach this code
                }
              }
            }
          }
        }
        
        msg.addField(field)
      })
      
      msgList += msg
      msgMap += (msg.name -> msg)
      
    })
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
  val parser = new NOMParser
  
  testNOM()
  
  def testNOM() {    
    parser.parse("test.json")
  
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
    
    
    // NOM parser test code
    val msgMap = parser.getMessageMap()
    
    val msg1 = msgMap.get("object1").get    
    val nom1 = msg1.createNOMInstance()
    
    nom1.setValue("field1", NDouble(0.1234))
    nom1.setValue("field2.basic1", NInteger(456))
    nom1.setValue("field2.enum1", NEnum("ENUMERATOR2"))
    val nom1Value = nom1.getValue("field1").toDouble()
    val nom1Value2 = nom1.getValue("field2.basic1").toInt()
    val nom1Value3 = nom1.getValue("field2.enum1").toInt()
    println("nom1::field1 : " + nom1Value)
    println("nom1::field2.basic1 : " + nom1Value2)
    println("nom1::field2.enum1 : " + nom1Value3)
    
    val msg2 = msgMap.get("object2").get
    val nom2 = msg2.createNOMInstance()
    
    nom2.setValue("field22.str1", NString("test"))
    nom2.setValue("field22.str2", NFixedString("testtest"))
    nom2.setValue("field22.float1", NFloat(333.444f))
    
    val nom2Value = nom2.getValue("field22.str1").toString()
    val nom2Value2 = nom2.getValue("field22.str2").toString()
    println("nom2::field22.str1 : " + nom2Value)
    println("nom2::field22.str2 : " + nom2Value2)
    
    // serialize / deserialize test
    println("----------------------------------------------------------------------")
    
    val data = nom1.serialize()
    val nom11 = msg1.createNOMInstance()
    nom11.deserialize(data._1, data._2)
    val nom11Value = nom11.getValue("field1").toDouble()
    val nom11Value2 = nom11.getValue("field2.basic1").toInt()
    val nom11Value3 = nom11.getValue("field2.enum1").toInt()
    println("nom11::field1 : " + nom11Value)
    println("nom11::field2.basic1 : " + nom11Value2)
    println("nom11::field2.enum1 : " + nom11Value3)
    
    
    val data2 = nom2.serialize()
    var nom22 = msg2.createNOMInstance()
    nom22.deserialize(data2._1, data2._2)
    nom22 = nom2.getClone()
    val nom22Value = nom22.getValue("field22.str1").toString()
    val nom22Value2 = nom22.getValue("field22.str2").toString()
    val nom22Value3 = nom22.getValue("field22.float1").toFloat()
    println("nom22::field22.str1 : " + nom22Value)
    println("nom22::field22.str2 : " + nom22Value2)
    println("nom22::field22.float1 : " + nom22Value3)
    
    assert(nom1Value == 0.1234)
  }
}