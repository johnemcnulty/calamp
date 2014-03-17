package jmcnulty

class MessageStructure[Field] {

  case class Element(field: Field, header: Boolean = false, children: List[Element] = List()) {

    def has(kids: Element*): Element = {
      this.copy(children = kids.toList)
    }
    def unary_~(): Element = {
      this.copy(header = true)
    }

  }
  
  implicit def fieldToElement(field: Field): Element = {
    new Element(field)
  }
}

