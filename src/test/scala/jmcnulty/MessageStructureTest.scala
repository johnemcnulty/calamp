package jmcnulty

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.google.common.io.BaseEncoding
import com.google.common.io.BaseEncoding.StandardBaseEncoding

@RunWith(classOf[JUnitRunner])
class MessageStructureTest extends FlatSpec {

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

    object Field extends Enumeration {
      val A, B, C, HEADER, D = Value
    }
    
    object Structure extends MessageStructure[Field.Value] {
      import Field._
      
      val structure = A has (~HEADER, B, C, D)

      val bytes = List(0x07.toByte)
      println("TADA! >>> " + structure)
    }
    Structure
  }

}