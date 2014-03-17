package jmcnulty

import java.nio.ByteBuffer
import scala.util.Try
import scala.util.Success

object Fields extends Enumeration {
  val MESSAGE, HEADER, HEADER_FLAGS, BODY, MOBILE_ID, MOBILE_ID_TYPE, AUTH_WORD, ROUTING, FORWARDING, REDIRECTION, MESSAGE_HEADER, SERVICE_TYPE, MESSAGE_TYPE, SEQ_NUM, BODY_CONTENT, NULL, ACK_NAK, EVENT_REPORT, ID_REPORT, USER_DATA, APP_DATA, CONFIG_PARAM, UNIT_REQUEST, LOCATE_REPORT, USER_DATA_ACCUMS, MINI_EVENT_REPORT, UPDATE_TIME, FIX_TIME, LATITUDE, LONGITUDE, ALTITUDE, SPEED, HEADING = Value
}
import Fields._

object CalAmpPreprocessor extends MessagePreprocessor[Fields.Value] {

  private val structure = MESSAGE has
    (HEADER has (~HEADER_FLAGS, MOBILE_ID, MOBILE_ID_TYPE, AUTH_WORD, ROUTING, FORWARDING, REDIRECTION),
      BODY has (~MESSAGE_HEADER has (SERVICE_TYPE, MESSAGE_TYPE, SEQ_NUM),
        BODY_CONTENT has (NULL, ACK_NAK, EVENT_REPORT)))

  def decode(bytes: Seq[Byte]): Try[Node] = {
    Success(parseElement(Context(), bytes, structure)._1)
  }

  def headerContext(byte: Byte): Context = {
    Context(Bitmap(MOBILE_ID, MOBILE_ID_TYPE, AUTH_WORD, ROUTING, FORWARDING, REDIRECTION).from(byte))
  }

  /*  val message = MESSAGE has
    (HEADER has (MOBILE_ID, MOBILE_ID_TYPE, AUTH_WORD, ROUTING, FORWARDING, REDIRECTION),
      BODY has (BODY_HEADER has (SERVICE_TYPE, MESSAGE_TYPE, SEQ_NUM),
        BODY_CONTENT has (UPDATE_TIME, FIX_TIME, LATITUDE, LONGITUDE, ALTITUDE, SPEED, HEADING)))
*/

  decode(HEADER_FLAGS) as { (context, bytes) =>
    if ((bytes.head & 0x80) > 0) (headerContext(bytes.head), bytes.tail)
    else (Empty(), bytes)
  }

  decode(MOBILE_ID) as variableLengthField(MOBILE_ID) _
  decode(MOBILE_ID_TYPE) as variableLengthField(MOBILE_ID_TYPE) _
  decode(AUTH_WORD) as variableLengthField(AUTH_WORD) _
  decode(ROUTING) as variableLengthField(ROUTING) _
  decode(FORWARDING) as variableLengthField(FORWARDING) _
  decode(REDIRECTION) as variableLengthField(REDIRECTION) _

  decode(SERVICE_TYPE) as { (context, bytes) =>
    bytes match {
      case Seq() => (Error(new Exception("Expected Service Type; found no bytes")), bytes)
      case b :: rest => (Context(SERVICE_TYPE, b.toInt), rest)
    }
  }
  decode(MESSAGE_TYPE) as { (context, bytes) =>
    bytes match {
      case Seq() => (Error(new Exception("Expected Message Type; found no bytes")), bytes)
      case b :: rest => (Context(MESSAGE_TYPE, b.toInt), rest)
    }
  }
  decode(SEQ_NUM) as { (context, bytes) =>
    bytes match {
      case a :: b :: rest => {
        (Context(SEQ_NUM, ByteBuffer.wrap(Array[Byte](0, 0, a, b)).getInt()), rest)
      }
      case _ => (Error(new Exception("Expected two byte sequence number; found " + bytes)), bytes)
    }
  }

  def getInt(context: Context, field: Fields.Value): Option[Int] = {
    context.get(field) match {
      case None => None
      case Some(p: Int) => Some(p)
      case _ => None
    }
  }

  decode(NULL) as { (context, bytes) =>
    getInt(context, MESSAGE_TYPE) match {
      case None => (Empty(), bytes)
      case Some(t) => if (t == 0) {
        bytes match {
          case Seq() => (Empty(), bytes)
          case a :: rest => (Error(new Exception("Expected a NULL message but found a message body: " + bytes)), bytes)
        }
      } else (Empty(), bytes)
    }
  }

  decode(ACK_NAK) as { (context, bytes) =>
    getInt(context, MESSAGE_TYPE) match {
      case None => (Empty(), bytes)
      case Some(t) => if (t == 1) {
        if (bytes.length < 6) {
          (Error(new Exception("Expected an ACK_NAK message of 6 bytes but found only " + bytes.length + " bytes: " + bytes)), bytes)
        } else {
          (Data(bytes.take(6)), bytes.drop(6))
        }
      } else (Empty(), bytes)
    }
  }

  decode(EVENT_REPORT) as { (context, bytes) =>
    getInt(context, MESSAGE_TYPE) match {
      case None => (Empty(), bytes)
      case Some(t) => if (t == 2) {
        if (bytes.length < 50) {
          (Error(new Exception("Expected an EVENT_REPORT message of 50 bytes but found only " + bytes.length + " bytes: " + bytes)), bytes)
        } else {
          (Data(bytes.take(50)), bytes.drop(50))
        }
      } else (Empty(), bytes)
    }
  }

}