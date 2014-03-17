package jmcnulty

import scala.util._
import java.nio.ByteBuffer

class MessagePreprocessor[Field] extends MessageStructure[Field] {
  trait Result
  
  type Directive = (Field, Any)

  class Bitmap(fields: Seq[Field]) {
    private val bits = List(0x01.toByte, 0x02.toByte, 0x04.toByte, 0x08.toByte, 0x10.toByte, 0x20.toByte, 0x40.toByte, 0x80.toByte)
    val pairs = bits zip fields

    def from(byte: Byte): Iterable[Field] = {
      pairs.filter(p => (byte & p._1) > 0) map { _._2 }
    }
  }
  object Bitmap {
    def apply(fields: Field*): Bitmap = {
      new Bitmap(fields)
    }
  }

  case class Context(directives: Iterable[Directive] = Set(), parent: Option[Context] = None) extends Result {
    private val map: Map[Field, Any] = directives.toMap
    def enabled(flag: Field): Boolean = {
      parent match {
        case None => map.contains(flag)
        case Some(p) => map.contains(flag) || p.enabled(flag)
      }
    }
    def enhances(other: Context): Context = {
      Context(directives, parent = Some(other))
    }
    def get(f: Field): Option[Any] = {
      map.get(f)
    }
  }

  object Context {
    def apply(f: Field, a: Any): Context = {
      new Context(Set((f, a)))
    }
    def apply(s: Directive*): Context = {
      new Context(s)
    }
    def apply(s: Iterable[Field]): Context = {
      new Context(s map { (_, true) })
    }
  }

  case class Data(bytes: Seq[Byte]) extends Result
  case class Error(val t: Throwable) extends Result
  case class Empty extends Result

  def error(msg: String)(context: Context, unparsed: Seq[Byte]): (Result, Seq[Byte]) = {
    (Error(new Exception(msg)), unparsed)
  }

  type Decoder = (Context, Seq[Byte]) => (Result, Seq[Byte])

  val decoders = scala.collection.mutable.Map[Field, Decoder]()

  class DecoderAssignment(field: Field) {
    def as(f: Decoder): DecoderAssignment = {
      decoders += (field -> f)
      DecoderAssignment.this
    }
  }

  def decode(field: Field): DecoderAssignment = {
    new DecoderAssignment(field)
  }

  trait Node {
    val field: Field
    val context: Context
    def pretty(level: Int): String
    override def toString(): String = {
      pretty(0)
    }
  }
  

  case class HeaderNode(field: Field, context: Context) extends Node {
    def pretty(level: Int): String = {
      " " * level + field + " = " + context
    }
  }

  case class DataNode(field: Field, context: Context, data: Data) extends Node {
    def pretty(level: Int): String = {
      " " * level + field + " = " + data
    }
  }
  case class InnerNode(field: Field, context: Context, children: List[Node]) extends Node {
    def pretty(level: Int): String = {
      " " * level + field + "(" + context + "):\n" + (children map { _.pretty(level + 2) }).mkString("\n")
    }
  }

  case class BadNode(field: Field, context: Context, msg: Throwable) extends Node {
    def pretty(level: Int): String = {
      "  " * level + List(field.toString, "Context: " + context.toString, "Error: " + msg).mkString(", ")
    }
  }
  case class OmittedNode(field: Field, context: Context) extends Node {
    def pretty(level: Int): String = {
      "  " * level + field + ": Not present"
    }
  }

  def parseInnerNode(context: Context, bytes: Seq[Byte], element: Element): (Node, Seq[Byte]) = {

    def parseChildren(context: Context, bytes: Seq[Byte], children: List[Element]): (List[Node], Seq[Byte]) = {
      children match {
        case List() => (List(), bytes)
        case child :: siblings => {
          val (node, remainder) = parseElement(context, bytes, child)
          node match {
            case n: BadNode => (List(n), remainder)
            case n: OmittedNode => parseChildren(context, remainder, siblings)
            case _ => {
              val (nodes, left) = parseChildren(context, remainder, siblings)
              (node :: nodes, left)
            }
          }
        }
      }
    }

    element.children match {
      case List() => (OmittedNode(element.field, context), bytes)
      case first :: siblings => {
        if (first.header) {
          val (node, remainder) = parseElement(context, bytes, first)
          node match {
            case HeaderNode(f, ctx) => {
              val subcontext = ctx enhances context
              val (kids, left) = parseChildren(subcontext, remainder, siblings)
              if (element.header) {
                val unified = kids.foldLeft(context)((ctx, kid) => new Context(ctx.directives ++ kid.context.directives))
                (HeaderNode(element.field, unified), left)
              } else
                (InnerNode(element.field, subcontext, kids), left)
            }
            case _ => (BadNode(first.field, context, new Exception("Parsing header field " + first.field + " produced non-header node" + node)), remainder)
          }
        } else {
          val (kids, left) = parseChildren(context, bytes, element.children)
          if (element.header) {
            val unified = kids.foldLeft(context)((ctx, kid) => new Context(ctx.directives ++ kid.context.directives))
            (HeaderNode(element.field, unified), left)
          } else
            (InnerNode(element.field, context, kids), left)
        }
      }
    }
  }

  def parseElement(context: Context, bytes: Seq[Byte], element: Element): (Node, Seq[Byte]) = {
    decoders.get(element.field) match {
      case None => parseInnerNode(context, bytes, element)

      case Some(decoder) => {
        decoder(context, bytes) match {
          case (c: Context, remainder) => (HeaderNode(element.field, c), remainder)
          case (d: Data, remainder) => (DataNode(element.field, new Context(parent = Some(context)), d), remainder)
          case (e: Error, remainder) => (BadNode(element.field, context, e.t), bytes)
          case (e: Empty, remainder) => (OmittedNode(element.field, context), bytes)
        }
      }
    }
  }
  
  def variableLengthField(field: Field)(context: Context, bytes: Seq[Byte]): (Result, Seq[Byte]) = {
    if (context.enabled(field)) {
      bytes match {
        case List() => (Error(new Exception("No bytes left to parse")), bytes)
        case h :: rest => {
          if (h.toInt <= rest.length) {
            val content = rest.take(h.toInt)
            (Data(rest.take(h.toInt)), rest.drop(h.toInt))
          } else {
            (Error(new Exception("Expected " + h.toInt + " bytes; found " + rest.length)), bytes)
          }
        }
      }
    } else (Empty(), bytes)
  }

  def fixedLengthField(field: Field, len: Int)(context: Context, bytes: Seq[Byte]): (Result, Seq[Byte]) = {
    if (context.enabled(field)) {
      bytes match {
        case Seq() => (Error(new Exception("No bytes left to parse")), bytes)
        case _ => {
          if (len <= bytes.length) {
            (Data(bytes.take(len)), bytes.drop(len))
          } else {
            (Error(new Exception("Expected " + len + " bytes; found " + bytes.length)), bytes)
          }
        }
      }
    } else (Empty(), bytes)
  }


  // TODO:  i can declare multiple decoders for fields and blindly overwrite
  // TODO:  should not have to repeat the field in a decoder assignment
}