package jmcnulty

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.google.common.io.BaseEncoding
import com.google.common.io.BaseEncoding.StandardBaseEncoding

class MessagePreprocessorTest extends FlatSpec {

  trait TestMessages {
    val base64Messages = List(
      "gwVDQgMUMQEBAQIAAU91IltPdSJcE6of+MY011EAACOyAAAABQAAByIAAP+hABAJAC2QEAAAAAAAAAAv+QAAADIAAACbAAAAAAAAADQAAACbAAAAAAAAAM8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGkAAAAD",
      "gwVDQgRIBgEBAQIAAVUn/kcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAv+DAADDCi+SEAAAAAAAAAAvBgAAAAAAAAAAAAAAAAAAABIAAAAAAAAAEgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
      "gwVEQQB3dgEBAQIFxVBiOppQYjqaGPJhQdEbD6oAALZKAAAJnwAjCQIAIP+rDwnJAB8DEAAFrS7PAAA3dABD9msBkmZDADQgdwCdVKQByckEAAGcmQII/Q8AAAYgAAAAAAAAAAAAAAAAAAAAAAAAAHMAAAAC",
      "gwVEQQIBKAEBAQIx11BiOptQYjqbEOHrEcTTvKoAADhtAAALeAC+DAIQVv+uDwjJAB8DEAADVWb3AAA23AA07kgA7CKTACVIpwBaNiQBCl+HAAAMzgFkiN0ACEN4AAAAAAAAAAAAAAAAAAAAAAAAAHYAAAAB",
      "gwVEQQIIEgEBAQIn11BiOptQYjqbGOIzQ9HstXUAAI8jAAABmAA6CQIICv+1HwrJACAEEAAFZsYrAAA2RAAX9KgAlB6qAD03BABVLCoAlDFZAAAMfwDpUQQAAAeHAAAAAAAAAAAAAAAAAAAAAAAAAH0AAAAD",
      "gwVEQQGFhAEBAQKRflBiOplQYjqZEz2u4MMJaLIAAVaEAAAHQgClDAICEf+eDwjJAB8DEAACjBN/AAA4pAATREIBfI1MAB9YSgAyoXgBfI60AAAPQgGvIOoAAAT8AAAAAAAAAAAAAAAAAAAAAAAAAGYAAAAC",
      "gwVEQQGFdwEBAQJMkFBiOptQYjqbEz3aDcMN5DQAAVbmAAAGEwCoDAICEf+bHwjJAB8DEAACwvtjAAA1rAAeSuMBgNE0ACMI0gBBWgMBgNHdAAADtgHCKCoAAFGVAAAAAAAAAAAAAAAAAAAAAAAAAGMAAAAB",
      "gwVDQgZZdAEBAQIOtFIdGQJSHRkCHK+4dMItMPQAAOuIAAAAAAASCwIGDP/CLwgJADw3EAABM4KoAAA1UQASuK8AzEAxAA+zvwAicIkAzGyuAAAAAADu3TcAAAATAAAAAAAAAAAAAAAAAAAAAAAAAIoAAAAA")

    def unwrap(base64: String): List[Byte] = {
      val v = if (base64.startsWith("0x")) base64.substring(2) else base64
      Base64Wrapper.decode(v).toList map { _.toByte }
    }
  }

  "A Context result in a header" should "produce only Context" in {
    object Fields extends Enumeration {
      val A, HEADER, B, C, D = Value
    }
    type Field = Fields.Value
    
    object SimpleMessage extends MessagePreprocessor[Field] {
      import Fields._

      val structure = A has (~HEADER, B, C, D)

      //decode(A) as { (context, bytes) => (Data(bytes), List()) } 
      decode(HEADER) as { (context, bytes) =>
        (Context(Bitmap(B, C, D).from(bytes.head)), bytes.tail)
      }

    }

    val bytes = List(0x07.toByte)
    println("TADA! >>> " + (SimpleMessage.structure))

  }

  /*
  "A message with high bit set" should "produce a header and a body" in {
    new TestMessages() {
      import Fields._

      val bytes = unwrap(base64Messages.head)
      //val bytes = List[Byte](0x83.toByte, 0x04, 0x01, 0x02, 0x03, 0x04, 0x5, 0x01, 0x02, 0x03, 0x04, 0x05, 0x1, 0x01, 0x1, 0x02, 0x1, 0x03, 0x1, 0x04)
      val message = MESSAGE has
        (HEADER has (~HEADER_FLAGS, MOBILE_ID, MOBILE_ID_TYPE, AUTH_WORD, ROUTING, FORWARDING, REDIRECTION),
          BODY has (~MESSAGE_HEADER has (SERVICE_TYPE, MESSAGE_TYPE, SEQ_NUM), 
              BODY_CONTENT has (NULL, ACK_NAK, EVENT_REPORT)))
//            BODY_CONTENT has (UPDATE_TIME, FIX_TIME, LATITUDE, LONGITUDE, ALTITUDE, SPEED, HEADING)))

      val msg = message from bytes
      println(msg.get)
    }
  }
*/

}