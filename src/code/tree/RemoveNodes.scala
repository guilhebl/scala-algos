package code.tree

case class TreeNode(nodes: Seq[TreeNode] = Seq.empty, data: Int = 0)

/**
  * https://careercup.com/question?id=5635485494411264
  *
  * @return
  */
object RemoveNodes {
  def main(args: Array[String]): Unit = {
    // Let us create the tree
    val root = TreeNode(
      nodes = Seq(
        TreeNode(nodes = Seq(
          TreeNode(data = 25), TreeNode(), TreeNode(data = 30), TreeNode()),
          data = 14),

        TreeNode(nodes = Seq(
          TreeNode(data = 18), TreeNode(), TreeNode(data = 11), TreeNode()),
          data = 6),

        TreeNode(nodes = Seq(
          TreeNode(data = 4), TreeNode(), TreeNode(data = 3), TreeNode())),

        TreeNode(nodes = Seq(
          TreeNode(), TreeNode(), TreeNode(data = 5), TreeNode(nodes = Seq(
            TreeNode(data = 7), TreeNode(), TreeNode(data = 9), TreeNode()))))
      ),
      data = 12
    )

    // Remove 0s
    val head = removeZeros(root)
    printTree(head)
  }

  def printTree(node: Option[TreeNode]): Unit = {
    node match {
      case Some(x) => printTree(x)
      case _ => println("Empty tree")
    }
  }

  def printTree(node: TreeNode): Unit = {
    println(node.data)
    node.nodes.foreach(printTree)
  }

  def getValidChildren(node: TreeNode) = {
    node.nodes.map(removeZeros).filter(_.isDefined).map(_.get)
  }

  def removeZeros(node: TreeNode): Option[TreeNode] = {
    if (node.data == 0 && node.nodes.isEmpty) {
      None
    }
    else if (node.data == 0 && node.nodes.nonEmpty) {
      val validChildren = getValidChildren(node)
      Some(
        TreeNode(
          data = validChildren.head.data,
          nodes = validChildren.drop(1)
        )
      )
    }
    else {
      Some(
        TreeNode(
          data = node.data,
          nodes = getValidChildren(node)
        )
      )
    }
  }

}
