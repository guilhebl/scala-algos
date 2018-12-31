package code.string

import scala.collection.mutable.ListBuffer

case class CharNode(char: Char, var prev: Option[CharNode] = None, var next: Option[CharNode] = None) {
  override def toString(): String = char.toString
}
case class Edit(edition: String, data: Option[CharNode] = None)

object TextEditorProgram {

  def main(args: Array[String]): Unit = {
    val t = new TextEditor()
    t.insertCharacter('a')
    t.insertCharacter('b')
    t.insertCharacter('c')
    println("RESULT = " + t.toString())

    t.moveCursorLeft()
    println("RESULT = " + t.toString())

    t.moveCursorLeft()
    println("RESULT = " + t.toString())

    t.backspace()
    println("RESULT = " + t.toString())

    t.moveCursorLeft()
    println("RESULT = " + t.toString())

    t.undo()
    println("RESULT = " + t.toString())

    t.undo()
    println("RESULT = " + t.toString())

    t.undo()
    println("RESULT = " + t.toString())

    t.undo()
    println("RESULT = " + t.toString())

    t.undo()
    println("RESULT = " + t.toString())
  }

}

class TextEditor {
  private val undo_stack = ListBuffer.empty[Edit]
  private val end = CharNode('\0')
  private var cursor = end

  def moveCursorLeft(): Unit = {
    if (cursor.prev.isDefined) {
      cursor = cursor.prev.get
      undo_stack.prepend(Edit("RIGHT"))
    }
  }

  def moveCursorRight(): Unit = {
    if (cursor ne end) {
      cursor = cursor.next.get
      undo_stack.prepend(Edit("LEFT"))
    }
  }

  def insertCharacter(ch: Char): Unit = {
    insert(CharNode(ch))
    undo_stack.prepend(Edit("DEL"))
  }

  def backspace(): Unit = {
    if (cursor.prev.isDefined) {
      undo_stack.prepend(Edit("INS", Some(delete(cursor.prev.get))))
    }
  }

  def undo(): Unit = {
    if (undo_stack.isEmpty) return
    val edit = undo_stack.head
    edit.edition match {
      case "LEFT" =>
        cursor = cursor.prev.get
      case "RIGHT" =>
        cursor = cursor.next.get
      case "DEL" =>
        delete(cursor.prev.get)
      case "INS" =>
        insert(edit.data.get)
      case _ => print("INVALID EDIT")
    }
  }

  override def toString: String = {
    val text = new StringBuilder
    var n = end.prev
    if (cursor == end) text.append('|')
    while (n.isDefined) {
      text.insert(0, n.get.char)
      if (cursor eq n.get) text.insert(0, '|')
      n = n.get.prev
    }
    text.toString
  }

  private def insert(node: CharNode): Unit = {
    val prev = cursor.prev
    node.next = Some(cursor)
    cursor.prev = Some(node)
    if (prev.isDefined) {
      prev.get.next = Some(node)
      node.prev = prev
    }
  }

  private def delete(del: CharNode): CharNode = {
    if (del.prev.isDefined) del.prev.get.next = Some(cursor)
    cursor.prev = del.prev
    del
  }
}