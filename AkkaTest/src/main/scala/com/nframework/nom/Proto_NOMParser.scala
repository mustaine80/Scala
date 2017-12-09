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
//  this information is not changable. need to mutable list?
//  usage: val intBE = BasicType("intBE", 4, "big", Int)
case class BasicType_Proto(length: Int, endian: String, primitives: AnyRef) extends TypeModel

case class EnumType_Proto(length: Int, enums: Map[String, Int]) extends TypeModel

case class ComplexType_Proto(name: String, model: TypeModel, size: Int) extends TypeModel

case class Field_Proto(name: String, models: Seq[TypeModel], size: Int, fixedLength: Int, indicator: Int)

//  Object inner variables are 'attribute'
case class Object_Proto(fields: Seq[Field_Proto], sharing: String, alignment: String)

//  Interaction inner variables are 'parameter'
case class Interaction_Proto(fields: Seq[Field_Proto], sharing: String, alignment: String)

object Proto_NOMParser extends Proto_Parser {
  var basicTypes = Map[String, BasicType_Proto]()
  var enumTypes = Map[String, EnumType_Proto]()
  var complexTypes = Map[String, Seq[ComplexType_Proto]]()
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

        basicTypes = composeBasicTypes(basicTypeMap)
        enumTypes = composeEnumTypes(enumTypeMap)
        complexTypes = composeComplexTypeList(complexTypeMap)
        objectTypes = composeObjectList(objectMap)
        interactionTypes = composeInteractionList(interactionMap)

        true
      } else { false }

    result
  }


  override def parse(path: String) : Boolean = {
    this.path = path
    parse()
  }


  def composeBasicTypes(m: Map[String, Map[String, String]]): Map[String, BasicType_Proto] = {
    val data = mutable.Map[String, BasicType_Proto]()

    m.map( ( e: (String, Map[String, String]) ) => {
      val name = e._1
      val length = e._2("length").toInt
      val endian = e._2("endian")
      val _type = e._2("type")

      (name, BasicType_Proto(length, endian, _type))
    }).foreach( data += _ )

    Map[String, BasicType_Proto]() ++ data
  }


  def composeEnumTypes(m: Map[String, Map[String, String]]): Map[String, EnumType_Proto] = {
    val data = mutable.Map[String, EnumType_Proto]()

    m.map( (e: (String, Map[String, String]) ) => {
      val name = e._1
      val length = e._2("length").toInt
      val enums = e._2.transform((x, y) => (y.toInt))

      (name, EnumType_Proto(length, enums))
    }).foreach( data += _ )

    Map[String, EnumType_Proto]() ++ data
  }


  def composeComplexTypeList(m: Map[String, Map[String, Map[String, String]]]): Map[String, Seq[ComplexType_Proto]] = {
    val data = mutable.Map[String, Seq[ComplexType_Proto]]()

    m.map((e: (String, Map[String, Map[String, String]])) => {
      val name = e._1
      val types = for {
        (alias, typeInfo) <- e._2
        model = extractTypeModel(typeInfo("type"))(0)   /// issue: use ComplexType recursively?
        size = typeInfo("size").toInt
      } yield ComplexType_Proto(alias, model, size)

      (name, types.toSeq)
    }).foreach( data += _)

    Map[String, Seq[ComplexType_Proto]]() ++ data
  }


  def composeObjectList(m: Map[String, Map[String, Map[String, String]]]): Map[String, Object_Proto] = {
    val data = mutable.Map[String, Object_Proto]()

    m.map( (e: (String, Map[String, Map[String, String]])) => {
      val name = e._1
      val fields = for {
        (alias, typeInfo) <- e._2
        if (alias != "sharing" && alias != "alignment")
          field = makeField(alias, typeInfo)
      } yield field

      val sharing = e._2("sharing").asInstanceOf[String]
      val alignment = e._2("alignment").asInstanceOf[String]

      (name, Object_Proto(fields.toSeq, sharing, alignment))
    }).foreach( data += _ )

      Map[String, Object_Proto]() ++ data
  }


  def composeInteractionList(m: Map[String, Map[String, Map[String, String]]]): Map[String, Interaction_Proto] = {
    val data = mutable.Map[String, Interaction_Proto]()

    m.map( (e: (String, Map[String, Map[String, String]])) => {
      val name = e._1
      val fields = for {
        (alias, typeInfo) <- e._2
        if (alias != "sharing" && alias != "alignment")
        field = makeField(alias, typeInfo)
      } yield field

      val sharing = e._2("sharing").asInstanceOf[String]
      val alignment = e._2("alignment").asInstanceOf[String]

      (name, Interaction_Proto(fields.toSeq, sharing, alignment))
    }).foreach( data += _ )

    Map[String, Interaction_Proto]() ++ data
  }


  def makeField(alias: String, typeInfo: Map[String, String]): Field_Proto = {
    val models = extractTypeModel(typeInfo("type"))
    val size = typeInfo("size").toInt
    val fixedLength = typeInfo("fixedLength").toInt
    val indicator = typeInfo("indicator").toInt

    Field_Proto(alias, models, size, fixedLength, indicator)
  }


  def extractTypeModel(hint: String): Seq[TypeModel] = {
        if (basicTypes.contains(hint))
          Seq[TypeModel](basicTypes(hint))
        else if (enumTypes.contains(hint))
          Seq[TypeModel](enumTypes(hint))
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