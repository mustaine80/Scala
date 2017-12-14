package com.nframework.nom

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import scala.collection.mutable
import scala.util.parsing.json.JSON

trait Proto_Parser {
  def parse() : Boolean
  def parse(path: String) : Boolean

  var path = ""
}


abstract class TypeModel

//  todo: need to PrimitiveModel ???
case class BasicType_Proto(length: Int, endian: String, primitives: AnyRef) extends TypeModel

case class EnumType_Proto(length: Int, enums: Map[String, Int]) extends TypeModel

case class ComplexType_Proto(models: Seq[(String, TypeModel, Int)]) extends TypeModel

case class Field_Proto(name: String, model: TypeModel, size: Int, fixedLength: Int, indicator: Int) extends TypeModel

case class Object_Proto(fields: Seq[Field_Proto], sharing: String, alignment: String) extends TypeModel

case class Interaction_Proto(fields: Seq[Field_Proto], sharing: String, alignment: String) extends TypeModel

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
    }.toSeq
    ComplexType_Proto(models)
  }


  def makeObjectType(typeInfo: Map[String, Map[String, String]]): Object_Proto = {
    val sharing = typeInfo("sharing").asInstanceOf[String]
    val alignment = typeInfo("alignment").asInstanceOf[String]
    val fields = (typeInfo - ("sharing", "alignment")).map{case (alias, info) => makeField(alias, info)}.toSeq
    Object_Proto(fields, sharing, alignment)
  }


  def makeInteractionType(typeInfo: Map[String, Map[String, String]]): Interaction_Proto = {
    val sharing = typeInfo("sharing").asInstanceOf[String]
    val alignment = typeInfo("alignment").asInstanceOf[String]
    val fields = (typeInfo - ("sharing", "alignment")).map{case (alias, info) => makeField(alias, info)}.toSeq
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