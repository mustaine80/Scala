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

  def getNValues(): List[NValueType] =  flatObjs(this).map{ x => convertNValue(x)
    .getOrElse(throw new Exception("convertNValue fail : " + x)) }  /// extractor
}


// !  변경된 데이터에 대해 기존 인스턴스 정보를 변경하지 않고 불변형의 신규 인스턴스를 생성하여 대체하는 전략을 사용한다.
object NomSerializer {
  val nomObjectTypeSerializer = serializer(objectTypes, _: NomSerializable, _: Int)
  val nomInteractionTypeSerializer = serializer(interactionTypes, _: NomSerializable, 0xFFFFFFFF)


  def serializer(schema: Map[String, Object_Proto], s: NomSerializable, updateFlag: Int): Array[Byte] = {
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


  def deserializer(s: NomSerializable, data: Array[Byte]): NomSerializable = {
    var offset = 0
    var lists = ListBuffer.empty[AnyRef]
    val flag = NInteger(0)

    offset += flag.deserialize(data, offset)
    var updateFlag = convertObj(flag).getOrElse(throw new Exception("deserializer's update flag convert error : " + flag)).asInstanceOf[Int]

    var marker = 1

    s.getNValues().foreach { x =>
      if ((updateFlag & marker) == marker)
        offset += x.deserialize(data, offset)

      lists += convertObj(x).getOrElse(throw new Exception("deserializer's lists convert error : " + x)).asInstanceOf[AnyRef]
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
  def getBasicType(primitive: String): Option[AnyRef] = {
    primitive match {
      case "Bool" => Some(false.asInstanceOf[AnyRef])
      case "Byte" => Some(0.toByte.asInstanceOf[AnyRef])
      case "Char" => Some('A'.asInstanceOf[AnyRef])
      case "Double" => Some(0.0.asInstanceOf[AnyRef])
      case "Float" => Some(0.0f.asInstanceOf[AnyRef])
      case "Integer" => Some(0.asInstanceOf[AnyRef])
      case "Short" => Some(0.toShort.asInstanceOf[AnyRef])
      case "String" => Some("".asInstanceOf[AnyRef])

      case _ => None
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
            case z: BasicType_Proto => { for (i <- 1 to size) yield getBasicType(z.primitive)
              .getOrElse(throw new Exception("getComplexType's getBasicType error: " + z.primitive)) }.toList
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
          yield getBasicType(z.primitive).getOrElse(throw new Exception("getDefaultNom's getBasicType error " + z.primitive))}

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

    val bar = old.getNValues().map{x => convertObj(x)
      .getOrElse(throw new Exception("old value convertObj fail! unknown type : " + x))} zip now.getNValues()
      .map{x => convertObj(x).getOrElse(throw new Exception("current value convertObj fail! unknown type : " + x))}
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


  //  todo: NBool()이 실패한다면?
  def convertNValue(o: Any): Option[NValueType] = {
    o match {
      case m: Boolean => Some(NBool(m))
      case m: Byte => Some(NByte(m))
      case m: Char => Some(NChar(m))
      case m: Double => Some(NDouble(m))
      case m: Float => Some(NFloat(m))
      case m: Int => Some(NInteger(m))
      case m: Short => Some(NShort(m))
      case m: String => Some(NString(m))

      case _ => None
    }
  }


  def convertObj(n: NValueType): Option[Any] = {
    n match {
      case m: NBool => Some(n.toShort())  //  todo: abstract method toBool() need...
      case m: NByte => Some(n.toByte())
      case m: NChar => Some(n.toChar())
      case m: NDouble => Some(n.toDouble())
      case m: NFloat => Some(n.toFloat())
      case m: NInteger => Some(n.toInt())
      case m: NShort => Some(n.toShort())
      case m: NString => Some(n.toString())

      case _ => None
    }
  }
}