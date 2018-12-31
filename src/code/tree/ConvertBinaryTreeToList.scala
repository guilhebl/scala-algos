package code.tree

case class TreeNodeDll(var left: TreeNodeDll = null, var right: TreeNodeDll = null, data: Int)

/**
  * https://careercup.com/question?id=5710647581474816
  *
  * @param node
  * @return
  */
object ConvertBinaryTreeToList {

  def main(args: Array[String]): Unit = {
    // Let us create the tree
    val root = TreeNodeDll(
          left = TreeNodeDll(
            left = TreeNodeDll(data = 25),
            right= TreeNodeDll(data = 30),
            data = 12
          ),
          right = TreeNodeDll(
            left = TreeNodeDll(data = 36),
            data = 15
          ),
          data = 10)

    // Convert to DLL
    val head = binTreeToList(root)

    // Print the converted list
    printList(head)
  }

  /* Function to print nodes in a given doubly linked list */
  private def printList(node: TreeNodeDll) = {
    var n = node
    while (n != null) {
      print(n.data + " ")
      n = n.right
    }
  }

  def binTreeToList(node: TreeNodeDll): TreeNodeDll = {
    if (node == null) {
      node
    } else {
      var newNode = binTreeToListUtil(node)
      while (newNode.left != null) newNode = newNode.left
      newNode
    }
  }

  def binTreeToListUtil(node: TreeNodeDll): TreeNodeDll = { // Base case
    if (node != null) {
      if (node.left != null) {
        var left = binTreeToListUtil(node.left)
        while (left.right != null) left = left.right
        left.right = node
        node.left = left
      }
      if (node.right != null) {
        var right = binTreeToListUtil(node.right)
        while (right.left != null) right = right.left
        right.left = node
        node.right = right
      }
    }
    node
  }

}
