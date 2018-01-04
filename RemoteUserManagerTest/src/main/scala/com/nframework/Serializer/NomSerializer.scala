package com.nframework.Serializer

import com.nframework.nom.Proto_NOMParser.{interactionTypes, objectTypes}
import com.nframework.nom._
import scala.collection.mutable.ListBuffer


/** nom schema 를 통한 자동 직렬화를 지원
  * !! getName 반환값을 NOM schema 내 object name 을 클래스명과 동일하게 작성해야 한다.
  */
trait NomSerializable extends Product {
  import NomSerializer._

  def getName(): String = getClass().getSimpleName   /// 반환값은 nom parser 에서 관리하는 object type key 로 사용

  def getValues(): List[NValueType] =  flatObjs(this).map{ x => convertObj(x) }  /// extractor
}


// !  변경된 데이터에 대해 기존 인스턴스 정보를 변경하지 않고 불변형의 신규 인스턴스를 생성하여 대체하는 전략을 사용한다.
object NomSerializer {
  val nomObjectTypeSerializer = nomSerializer(objectTypes, _: NomSerializable, _: Int)
  val nomInteractionTypeSerializer = nomSerializer(interactionTypes, _: NomSerializable, 0xFFFFFFFF)


  def nomSerializer(schema: Map[String, Object_Proto], s: NomSerializable, updateFlag: Int): Array[Byte] = {
    val noms = getNOM(schema(s.getName())) zip s.getValues()
    var marker = 1

    noms.map{ x =>
      if ((updateFlag & marker) == marker) {
        marker = marker << 1
        x._1.setValue(x._2)
        x._1.serialize()._1
      }
      else {
        marker = marker << 1
        Array.empty[Byte]
      }
    }.foldLeft(NInteger(updateFlag).serialize()._1)(_ ++ _)
  }


  def nomDeserializer(s: NomSerializable, data: Array[Byte]): NomSerializable = {
    var offset = 0
    var lists = ListBuffer.empty[AnyRef]
    val bar = NInteger(0)

    offset += bar.deserialize(data, offset)
    var updateFlag = convertNValue(bar).asInstanceOf[Int]

    var marker = 1

    s.getValues().foreach { x =>
      if ((updateFlag & marker) == marker)
        offset += x.deserialize(data, offset)

      lists += convertNValue(x).asInstanceOf[AnyRef]
      marker = marker << 1
    }

    val args = s.productIterator.toList.map {
      case m: NomSerializable =>
        val (obj, other) = lists.splitAt(m.productArity)
        lists = other
        makeNomSerializable(m.getName(), obj.toList).asInstanceOf[AnyRef]
      case m: Any => lists.remove(0)
    }

    makeNomSerializable(s.getName(), args)
  }


  def makeNomSerializable(name: String, args: List[AnyRef]): NomSerializable = {
    val constructor = Class.forName("com.nframework." + name).getConstructors()(0)
    constructor.newInstance(args:_*).asInstanceOf[NomSerializable]
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


  def getComplexTypeNOM(models: Seq[(String, TypeModel, Int, Int)]): List[NValueType] = {
    val nom = for (m <- models) yield getNValue(m._2, m._3)
    nom.flatten.toList
  }


  def getNOM(object_proto: Object_Proto): List[NValueType] = {
    val nom = for (f <- object_proto.fields) yield getNValue(f.model, f.size)
    nom.flatten
  }


  def getNValue(t: TypeModel, size: Int): List[NValueType] = {
    t match {
      case z: BasicType_Proto => {for (i <- 1 to size) yield getBasicTypeNOM(z.primitive)}.toList
      case z: EnumType_Proto => {for (i <- 1 to size) yield getEnumTypeNOM(z.enums)}.toList.flatten
      case z: ComplexType_Proto => {for (i <- 1 to size) yield getComplexTypeNOM(z.models)}.toList.flatten
    }
  }


  //  NOM schema 정보를 기반으로 NOMSerializable 객체를 생성한다. 이 정보는 DiscoverMap 에 등록할 기본 객체 정보를 반환한다.
  def getBasicTypeNOMSerializable(primitive: String): AnyRef = {
    primitive match {
      case "Integer" => 0.asInstanceOf[AnyRef]
      case "Double" => 0.0.asInstanceOf[AnyRef]
      case "String" => "".asInstanceOf[AnyRef]

      case _ => println("[NOM parser] getBasicTypeNOMSerializable fail! unknown type." + primitive); 0.asInstanceOf[AnyRef]
    }
  }


  def getEnumTypeNOMSerializable(enums: Map[String, Int]): List[AnyRef] = {
    (for (e <- enums) yield e._2.asInstanceOf[AnyRef]).toList
  }


  def getComplexTypeNOMSerializable(name: String, models: List[(String, TypeModel, Int, Int)]): NomSerializable = {
    val args = models.flatMap{
      case (_, model, size, ind) =>
        for {
          i <- 0 to models.size
          if ind == i
        } yield {
          model match {
            case z: BasicType_Proto => { for (i <- 1 to size) yield getBasicTypeNOMSerializable(z.primitive) }.toList
            case z: EnumType_Proto => { for (i <- 1 to size) yield getEnumTypeNOMSerializable(z.enums) }.flatten
          }
        }
    }.flatten

    makeNomSerializable(name, args)
  }


  def getDefaultNOMSerializable(msgName: String): NomSerializable = {
    val object_proto = objectTypes.getOrElse(msgName, interactionTypes(msgName))

    val args = { for {
      i <- 0 to object_proto.fields.size
      f <- object_proto.fields
      if f.indicator == i
    } yield {
      f.model match {
        case z: BasicType_Proto => {for (i <- 1 to f.size)
          yield getBasicTypeNOMSerializable(z.primitive)}

        case z: EnumType_Proto => {for (i <- 1 to f.size)
          yield getEnumTypeNOMSerializable(z.enums)}

        case z: ComplexType_Proto => {for (i <- 1 to f.size)
          yield getComplexTypeNOMSerializable(f.modelName, z.models).asInstanceOf[AnyRef]}
      }
    }}.toList.flatten

    makeNomSerializable(msgName, args)
  }


  //  update object 의 실제 변경된 필드를 Marking 하는 flag 를 반환한다.
  def compareObject(old: NomSerializable, now: NomSerializable): Int = {
    var updateFlag = 0
    var marker = 1

    val bar = old.getValues().map{x => convertNValue(x)} zip now.getValues().map{x => convertNValue(x)}
    bar.foreach{ x =>
      if (x._1 != x._2) updateFlag += marker

      marker = marker << 1
    }

    updateFlag
  }


  //  helper
  def flatObjs(nom: NomSerializable): List[Any] = {
    nom.productIterator.map{
      case m: NomSerializable => m.productIterator.toList
      case m: Any => m :: Nil
    }.toList.flatten
  }


  def convertObj(o: Any): NValueType = {  //  todo: NValueType 도 Nil 을 지원해야 할 듯
    o match {
      case m: Boolean => NBool(m)
      case m: Byte => NByte(m)
      case m: Char => NChar(m)
      case m: Double => NDouble(m)
      case m: Float => NFloat(m)
      case m: Int => NInteger(m)
      case m: Short => NShort(m)
      case m: String => NString(m)
      case _ => println("convertObj error!!! " + o); NBool(false)
    }
  }


  def convertNValue(nom: NValueType): Any = {
    nom match {
      case m: NBool => nom.toShort()  //  abstract method toBool() need...
      case m: NByte => nom.toByte()
      case m: NChar => nom.toChar()
      case m: NDouble => nom.toDouble()
      case m: NFloat => nom.toFloat()
      case m: NInteger => nom.toInt()
      case m: NShort => nom.toShort()
      case m: NString => nom.toString()

      case _ => println("[NOM parser] nValueMatcher fail! unknown type : " + nom); Nil
    }
  }
}