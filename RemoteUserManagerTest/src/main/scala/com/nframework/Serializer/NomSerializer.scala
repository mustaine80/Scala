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

  def getNValues(): List[NValueType] =  flatObjs(this).map{ x => convertNValue(x) }  /// extractor
}


// !  변경된 데이터에 대해 기존 인스턴스 정보를 변경하지 않고 불변형의 신규 인스턴스를 생성하여 대체하는 전략을 사용한다.
object NomSerializer {
  val nomObjectTypeSerializer = nomSerializer(objectTypes, _: NomSerializable, _: Int)
  val nomInteractionTypeSerializer = nomSerializer(interactionTypes, _: NomSerializable, 0xFFFFFFFF)


  def nomSerializer(schema: Map[String, Object_Proto], s: NomSerializable, updateFlag: Int): Array[Byte] = {
    val nValues = s.getNValues()
    var marker = 1

    nValues.map{ x =>
      if ((updateFlag & marker) == marker) {
        marker = marker << 1
        x.serialize()._1
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
    val flag = NInteger(0)

    offset += flag.deserialize(data, offset)
    var updateFlag = convertObj(flag).asInstanceOf[Int]

    var marker = 1

    s.getNValues().foreach { x =>
      if ((updateFlag & marker) == marker)
        offset += x.deserialize(data, offset)

      lists += convertObj(x).asInstanceOf[AnyRef]
      marker = marker << 1
    }

    val args = s.productIterator.toList.map {
      case m: NomSerializable =>
        val (obj, other) = lists.splitAt(m.productArity)
        lists = other
        makeNom(m.getName(), obj.toList).asInstanceOf[AnyRef]
      case m: Any => lists.remove(0)
    }

    makeNom(s.getName(), args)
  }


  def makeNom(name: String, args: List[AnyRef]): NomSerializable = {
    val constructor = Class.forName("com.nframework." + name).getConstructors()(0)
    constructor.newInstance(args:_*).asInstanceOf[NomSerializable]
  }


  //  NOM schema 정보를 기반으로 NOMSerializable 객체를 생성한다. 이 정보는 DiscoverMap 에 등록할 기본 객체 정보를 반환한다.
  def getBasicType(primitive: String): AnyRef = {
    primitive match {
      case "Bool" => false.asInstanceOf[AnyRef]
      case "Byte" => 0.toByte.asInstanceOf[AnyRef]
      case "Char" => 'A'.asInstanceOf[AnyRef]
      case "Double" => 0.0.asInstanceOf[AnyRef]
      case "Float" => 0.0f.asInstanceOf[AnyRef]
      case "Integer" => 0.asInstanceOf[AnyRef]
      case "Short" => 0.toShort.asInstanceOf[AnyRef]
      case "String" => "".asInstanceOf[AnyRef]

      case _ => println("[NOM parser] getBasicTypeNOMSerializable fail! unknown type." + primitive); 0.asInstanceOf[AnyRef]
    }
  }


  def getEnumType(enums: Map[String, Int]): List[AnyRef] = {
    (for (e <- enums) yield e._2.asInstanceOf[AnyRef]).toList
  }


  def getComplexType(name: String, models: List[(String, TypeModel, Int, Int)]): NomSerializable = {
    val args = models.flatMap{
      case (_, model, size, ind) =>
        for {
          i <- 0 to models.size
          if ind == i
        } yield {
          model match {
            case z: BasicType_Proto => { for (i <- 1 to size) yield getBasicType(z.primitive) }.toList
            case z: EnumType_Proto => { for (i <- 1 to size) yield getEnumType(z.enums) }.flatten
          }
        }
    }.flatten

    makeNom(name, args)
  }


  def getDefaultNom(msgName: String): NomSerializable = {
    //  todo: OrElse 후 interactionTypes 에 대한 getOrElse 처리
    val object_proto = objectTypes.getOrElse(msgName, interactionTypes(msgName))

    val args = { for {
      i <- 0 to object_proto.fields.size
      f <- object_proto.fields
      if f.indicator == i
    } yield {
      f.model match {
        case z: BasicType_Proto => {for (i <- 1 to f.size)
          yield getBasicType(z.primitive)}

        case z: EnumType_Proto => {for (i <- 1 to f.size)
          yield getEnumType(z.enums)}

        case z: ComplexType_Proto => {for (i <- 1 to f.size)
          yield getComplexType(f.modelName, z.models).asInstanceOf[AnyRef]}
      }
    }}.toList.flatten

    makeNom(msgName, args)
  }


  //  update object 의 실제 변경된 필드를 Marking 하는 flag 를 반환한다.
  def compareObject(old: NomSerializable, now: NomSerializable): Int = {
    var updateFlag = 0
    var marker = 1

    val bar = old.getNValues().map{x => convertObj(x)} zip now.getNValues().map{x => convertObj(x)}
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


  def convertNValue(o: Any): NValueType = {  //  todo: NValueType 도 Nil 을 지원해야 할 듯
    o match {
      case m: Boolean => NBool(m)
      case m: Byte => NByte(m)
      case m: Char => NChar(m)
      case m: Double => NDouble(m)
      case m: Float => NFloat(m)
      case m: Int => NInteger(m)
      case m: Short => NShort(m)
      case m: String => NString(m)
      case _ => println("convertNValue error!!! " + o); NBool(false)
    }
  }


  def convertObj(n: NValueType): Any = {
    n match {
      case m: NBool => n.toShort()  //  abstract method toBool() need...
      case m: NByte => n.toByte()
      case m: NChar => n.toChar()
      case m: NDouble => n.toDouble()
      case m: NFloat => n.toFloat()
      case m: NInteger => n.toInt()
      case m: NShort => n.toShort()
      case m: NString => n.toString()

      case _ => println("[NOM parser] convertObj fail! unknown type : " + n); Nil
    }
  }
}