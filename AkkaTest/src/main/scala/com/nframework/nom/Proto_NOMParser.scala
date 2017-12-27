package com.nframework.nom

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import scala.collection.mutable
import scala.util.parsing.json.JSON

trait Proto_Parser {
  def parse() : Boolean
  def parse(path: String) : Boolean

  var path = ""
}


/** nom schema 를 통한 자동 직렬화를 지원
  * !! getName 반환값을 NOM schema 내 object name 을 클래스명과 동일하게 작성해야 한다.
  */
trait NomSerializable {
  def getName(): String = getClass().getSimpleName   /// 반환값은 nom parser 에서 관리하는 object type key 로 사용

  def getDefault = setValues(getValues(): _*)   /// User Manager 내 Discover map 에 등록하기 위한 dummy 객체 제공

  def getValues(): List[NValueType] /// nom parser 에서 관리하는 object type 에 대한 mapping 정보 제공

  def setValues(ns: NValueType*): NomSerializable   /// 역직렬화 시 객체 replication 을 위해 필요
}


abstract class TypeModel

case class BasicType_Proto(length: Int, endian: String, primitive: String) extends TypeModel

case class EnumType_Proto(length: Int, enums: Map[String, Int]) extends TypeModel

case class ComplexType_Proto(models: List[(String, TypeModel, Int)]) extends TypeModel

case class Field_Proto(name: String, model: TypeModel, size: Int, fixedLength: Int, indicator: Int) extends TypeModel

case class Object_Proto(fields: List[Field_Proto], sharing: String, alignment: String) extends TypeModel

case class Interaction_Proto(fields: List[Field_Proto], sharing: String, alignment: String) extends TypeModel

object Proto_NOMParser extends Proto_Parser {
  var basicTypes = Map[String, BasicType_Proto]()
  var enumTypes = Map[String, EnumType_Proto]()
  var complexTypes = Map[String, ComplexType_Proto]()
  var objectTypes = Map[String, Object_Proto]()
  var interactionTypes = Map[String, Interaction_Proto]()

  override def parse(): Boolean = {
    val fileutf8 = new BufferedReader(new InputStreamReader(new FileInputStream(new File(path)), "UTF-8") )

    var line = ""
    var jsonstrutf8 = ""

    line = fileutf8.readLine

    while(line != null) {
      jsonstrutf8 += line
      line = fileutf8.readLine
    }

    val json = JSON.parseFull(jsonstrutf8)

    val result : Boolean =
      if (json != None) {
        val root = json.get.asInstanceOf[Map[String, Any]]
        val basicTypeMap = root("BasicTypes").asInstanceOf[Map[String, Map[String, String]]]
        val enumTypeMap = root("EnumTypes").asInstanceOf[Map[String, Map[String, String]]]
        val complexTypeMap = root("ComplexTypes").asInstanceOf[Map[String, Map[String, Map[String, String]]]]
        val objectMap = root("Objects").asInstanceOf[Map[String, Map[String, Map[String, String]]]]
        val interactionMap = root("Interactions").asInstanceOf[Map[String, Map[String, Map[String, String]]]]

        basicTypes = composeModel(basicTypeMap, makeBasicType)
        enumTypes = composeModel(enumTypeMap, makeEnumType)
        complexTypes = composeModel(complexTypeMap, makeComplexType)
        objectTypes = composeModel(objectMap, makeObjectType)
        interactionTypes = composeModel(interactionMap, makeInteractionType)

        true
      } else { false }

    result
  }


  override def parse(path: String) : Boolean = {
    this.path = path
    parse()
  }


  def composeModel[T, U <: TypeModel](schema: Map[String, T], proc: T => U): Map[String, U] = {
    val data = mutable.Map[String, U]()
    schema.map{
      case (name, typeInfo) => (name, proc(typeInfo))
    }.foreach( data += _ )
    Map[String, U]() ++ data
  }


  def makeBasicType(typeInfo: Map[String, String]): BasicType_Proto = {
    val length = typeInfo("length").toInt
    val endian = typeInfo("endian")
    val primitive = typeInfo("type")
    BasicType_Proto(length, endian, primitive)
  }


  def makeEnumType(typeInfo: Map[String, String]): EnumType_Proto = {
    val length = typeInfo("length").toInt
    val enums = typeInfo.transform((x, y) => (y.toInt))
    EnumType_Proto(length, enums)
  }


  def makeComplexType(typeInfo: Map[String, Map[String, String]]): ComplexType_Proto = {
    val models = typeInfo.map{
      case(name, _type) => {
        val model = extractTypeModel(_type("type"))
        val size = _type("size").toInt
        (name, model, size)
      }
    }.toList
    ComplexType_Proto(models)
  }


  def makeObjectType(typeInfo: Map[String, Map[String, String]]): Object_Proto = {
    val sharing = typeInfo("sharing").asInstanceOf[String]
    val alignment = typeInfo("alignment").asInstanceOf[String]
    val fields = (typeInfo - ("sharing", "alignment")).map{case (alias, info) => makeField(alias, info)}.toList
    Object_Proto(fields, sharing, alignment)
  }


  def makeInteractionType(typeInfo: Map[String, Map[String, String]]): Interaction_Proto = {
    val sharing = typeInfo("sharing").asInstanceOf[String]
    val alignment = typeInfo("alignment").asInstanceOf[String]
    val fields = (typeInfo - ("sharing", "alignment")).map{case (alias, info) => makeField(alias, info)}.toList
    Interaction_Proto(fields, sharing, alignment)
  }


  def makeField(alias: String, typeInfo: Map[String, String]): Field_Proto = {
    val model = extractTypeModel(typeInfo("type"))
    val size = typeInfo("size").toInt
    val fixedLength = typeInfo("fixedLength").toInt
    val indicator = typeInfo("indicator").toInt
    Field_Proto(alias, model, size, fixedLength, indicator)
  }


  def extractTypeModel(hint: String): TypeModel = {
        if (basicTypes.contains(hint))
          basicTypes(hint)
        else if (enumTypes.contains(hint))
          enumTypes(hint)
        else
          complexTypes(hint)
   }


  //  NOM schema 정보를 기반으로 NOM 객체를 생성한다. 이 정보는 NMessage 내 NOM 객체를 직렬화하는데 사용한다.
  def getBasicTypeNOM(primitive: String): NValueType = {
    primitive match {
      case "Bool" => NBool(false)
      case "Byte" => NByte(0.toByte)
      case "Char" => NChar('A')
      case "Double" => NDouble(0.0)
      case "Float" => NFloat(0.0f)
      case "Integer" => NInteger(0)
      case "Short" => NShort(0.toShort)
      case "String" => NString("")
      case _ => println("[NOM parser] getBasicTypeNOM fail! unknown type."); NInteger(0)
    }
  }


  def getEnumTypeNOM(enums: Map[String, Int]): List[NValueType] = {
    val nom = for (e <- enums) yield NEnum(e._1, e._2)
    nom.toList
  }


  def getComplexTypeNOM(models: Seq[(String, TypeModel, Int)]): List[NValueType] = {
    val nom = for (m <- models) yield getNOMValue(m._2, m._3)
    nom.flatten.toList
  }


  def getObjectTypeNOM(object_proto: Object_Proto): List[NValueType] = {
    val nom = for (f <- object_proto.fields) yield getNOMValue(f.model, f.size)
    nom.flatten
  }


  def getInteractionTypeNOM(interaction_proto: Interaction_Proto): List[NValueType] = {
    val nom = for (f <- interaction_proto.fields) yield getNOMValue(f.model, f.size)
    nom.flatten
  }


  def getNOMValue(t: TypeModel, size: Int): List[NValueType] = {
    t match {
      case z: BasicType_Proto => {for (i <- 1 to size) yield getBasicTypeNOM(z.primitive)}.toList
      case z: EnumType_Proto => {for (i <- 1 to size) yield getEnumTypeNOM(z.enums)}.toList.flatten
      case z: ComplexType_Proto => {for (i <- 1 to size) yield getComplexTypeNOM(z.models)}.toList.flatten
    }
  }

  //  todo: 일단 BasicType 만 지원하도록 구현한다. getValues() 에서 complext type 을 처리해서 넘겨줘야 한다.
  def nomObjectTypeSerializer(s: NomSerializable): Array[Byte] = {
    var data = Array.empty[Byte]
    val obj = getObjectTypeNOM(objectTypes(s.getName())) zip s.getValues()

    obj foreach { x =>
      x._1.setValue(x._2)
      data = data ++ x._1.serialize()._1
    }

    data
  }


  def nomObjectTypeDeserializer(s: NomSerializable, data: Array[Byte]): NomSerializable = {
    var offset = 0
    var lists = List.empty[NValueType]

    val obj = getObjectTypeNOM(objectTypes(s.getName())) zip s.getValues()

    obj foreach { x =>
      offset += x._2.deserialize(data, offset)
      lists = lists :+ x._2
    }

    s.setValues(lists: _*)
  }


  def nomInteractionTypeSerializer(s: NomSerializable): Array[Byte] = {
    var data = Array.empty[Byte]
    val interaction = getInteractionTypeNOM(interactionTypes(s.getName())) zip s.getValues()

    interaction foreach { x =>
      x._1.setValue(x._2)
      data = data ++ x._1.serialize()._1
    }

    data
  }


  def nomInteractionTypeDeserializer(s: NomSerializable, data: Array[Byte]): NomSerializable = {
    var offset = 0
    var lists = List.empty[NValueType]

    val interaction = getInteractionTypeNOM(interactionTypes(s.getName())) zip s.getValues()

      interaction foreach { x =>
      offset += x._2.deserialize(data, offset)
      lists = lists :+ x._2
    }

    s.setValues(lists: _*)
  }
}


object Proto_Parser_Test extends App {
  println("JSON Test!")

  Proto_NOMParser.parse("src/main/Resources/test.json")

  println(Proto_NOMParser.basicTypes)
  println(Proto_NOMParser.enumTypes)
  println(Proto_NOMParser.complexTypes)
  println(Proto_NOMParser.objectTypes)
  println(Proto_NOMParser.interactionTypes)
}