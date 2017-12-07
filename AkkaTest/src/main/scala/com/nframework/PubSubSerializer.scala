package com.nframework

import akka.serialization.Serializer
import com.nframework.meb.DummyNOM
import com.nframework.mec._
import com.nframework.nom.{NChar, NOM, NValueType}

class PubSubSerializer extends Serializer {
  override def includeManifest: Boolean = false

  override def identifier: Int = 12345

  def nValueTypeSerialize[U <: NValueType](v: U): Array[Byte] = {
    val pathLength = Array[Byte](v.path.toByte)
    val path = v.path.getBytes
    //  nom
    val bigEndian = v.bigEndian.toString.toByte
    val signed = v.signed.toString.toByte
    val length = v.length.toByte
    val variable = v.variable.toString.toByte
    val typeLength = v.typeLength.toByte
    //  dataType

    pathLength ++ path ++ (bigEndian :: signed :: length :: variable :: typeLength :: Nil)
  }

  //  todo: use to nom::serialize()
  def nomSerialize(nom: NOM): Array[Byte] = {
    nom match {
      case n: DummyNOM => {
        val objNameLength = Array[Byte](n.objName.size.toByte)
        val objName = n.objName.getBytes()
        val nVal = n.value match {
          case c: NChar => {
            val nValue = nValueTypeSerialize(c)
            val value = c.value.toByte

            nValue ++ (value :: Nil)
          }

          case _ => Array.empty[Byte]
        }

        objNameLength ++ objName ++ nVal
      }

      case _ => Array.empty[Byte]
    }
  }


  override def toBinary(o: AnyRef): Array[Byte] = {
    o match {
      case m: RegisterMsg => {
          val ID = Array[Byte](101)
          val msgNameLength = m.msgName.size.toByte
          val userNameLength = m.userName.size.toByte
          val msgName = m.msgName.getBytes()
          val userName = m.userName.getBytes()
          ID ++ (msgNameLength :: userNameLength :: Nil) ++ msgName ++ userName
        }

      case m: UpdateMsg => {
        val ID = Array[Byte](102)
        ID ++ nomSerialize(m.msg)
      }

      case m: SendMsg => {
        val ID = Array[Byte](103)
        ID ++ nomSerialize(m.msg)
      }

      case m: DeleteMsg => {
        val ID = Array[Byte](104)
        ID ++ nomSerialize(m.msg)
      }

      case m: DiscoverMsg => {
        val ID = Array[Byte](105)
//        ID ++ nomSerialize(m.msg)
        ID
      }

      case m: ReflectMsg => {
        val ID = Array[Byte](106)
        val msg = nomSerialize(m.msg)
        val buffer = m.buffer   /// todo: why use to buffer?

        ID ++ msg ++ (buffer :: Nil)
      }

      case m: RecvMsg => {
        val ID = Array[Byte](107)
        ID ++ nomSerialize(m.msg)
      }

      case m: RemoveMsg => {
        val ID = Array[Byte](108)
        ID ++ nomSerialize(m.msg)
      }

      case _ => Array.empty[Byte]
      }
  }

  //  todo: use head
  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    bytes(0).toInt match {
      case 105 => {
          DiscoverMsg(DummyNOM("CBIT", NChar('a')))
      }

      case _ => "not implement custom deserialize"
    }
  }

  }
