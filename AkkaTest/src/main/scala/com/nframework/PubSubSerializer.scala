package com.nframework

import java.nio.ByteBuffer

import akka.serialization.SerializerWithStringManifest
import boopickle.Default._
import com.nframework.mec.MEC_Proto.{MebAttatch, PubSubInfo, PubSubInfoForwarding}
import com.nframework.mec._

class PubSubSerializer extends SerializerWithStringManifest {
  override def identifier: Int = 12345

  val RegisterMsgManifest = classOf[RegisterMsg].getName
  val UpdateMsgManifest = classOf[UpdateMsg].getName
  val SendMsgManifest = classOf[SendMsg].getName
  val DeleteMsgManifest = classOf[DeleteMsg].getName
  val ReflectMsgManifest = classOf[ReflectMsg].getName
  val DiscoverMsgManifest = classOf[DiscoverMsg].getName
  val RecvMsgManifest = classOf[RecvMsg].getName
  val RemoveMsgManifest = classOf[RemoveMsg].getName

  val MebAttatchManifest = classOf[MebAttatch].getName
  val PubSubInfoManifest = classOf[PubSubInfo].getName
  val PubSubInfoForwardingManifest = classOf[PubSubInfoForwarding].getName

  override def manifest(o: AnyRef): String = o match {
    case _: RegisterMsg => RegisterMsgManifest
    case _: UpdateMsg => UpdateMsgManifest
    case _: SendMsg => SendMsgManifest
    case _: DeleteMsg => DeleteMsgManifest
    case _: ReflectMsg => ReflectMsgManifest
    case _: DiscoverMsg => DiscoverMsgManifest
    case _: RecvMsg => RecvMsgManifest
    case _: RemoveMsg => RemoveMsgManifest

    case _: MebAttatch => MebAttatchManifest
    case _: PubSubInfo => PubSubInfoManifest
    case _: PubSubInfoForwarding => PubSubInfoForwardingManifest
  }

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case m: RegisterMsg => Pickle.intoBytes(m).array()
    case m: UpdateMsg => Pickle.intoBytes(m).array()
    case m: SendMsg => Pickle.intoBytes(m).array()
    case m: DeleteMsg => Pickle.intoBytes(m).array()
    case m: ReflectMsg => Pickle.intoBytes(m).array()
    case m: DiscoverMsg => Pickle.intoBytes(m).array()
    case m: RecvMsg => Pickle.intoBytes(m).array()
    case m: RemoveMsg => Pickle.intoBytes(m).array()

    case m: MebAttatch => Pickle.intoBytes(m).array()
    case m: PubSubInfo => Pickle.intoBytes(m).array()
    case m: PubSubInfoForwarding => Pickle.intoBytes(m).array()
  }

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = manifest match {
    case RegisterMsgManifest => Unpickle[RegisterMsg].fromBytes(ByteBuffer.wrap(bytes))
    case UpdateMsgManifest => Unpickle[UpdateMsg].fromBytes(ByteBuffer.wrap(bytes))
    case SendMsgManifest => Unpickle[SendMsg].fromBytes(ByteBuffer.wrap(bytes))
    case DeleteMsgManifest => Unpickle[DeleteMsg].fromBytes(ByteBuffer.wrap(bytes))
    case ReflectMsgManifest => Unpickle[ReflectMsg].fromBytes(ByteBuffer.wrap(bytes))
    case DiscoverMsgManifest => Unpickle[DiscoverMsg].fromBytes(ByteBuffer.wrap(bytes))
    case RecvMsgManifest => Unpickle[RecvMsg].fromBytes(ByteBuffer.wrap(bytes))
    case RemoveMsgManifest => Unpickle[RemoveMsg].fromBytes(ByteBuffer.wrap(bytes))

    case MebAttatchManifest => Unpickle[MebAttatch].fromBytes(ByteBuffer.wrap(bytes))
    case PubSubInfoManifest => Unpickle[PubSubInfo].fromBytes(ByteBuffer.wrap(bytes))
    case PubSubInfoForwardingManifest => Unpickle[PubSubInfoForwarding].fromBytes(ByteBuffer.wrap(bytes))
  }

  implicit val pubSubPickler = boopickle.CompositePickler[PubSub]
    .addConcreteType[RegisterMsg]
    .addConcreteType[UpdateMsg]
    .addConcreteType[SendMsg]
    .addConcreteType[DeleteMsg]
    .addConcreteType[ReflectMsg]
    .addConcreteType[DiscoverMsg]
    .addConcreteType[RecvMsg]
    .addConcreteType[RemoveMsg]
}
