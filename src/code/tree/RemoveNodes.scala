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
          data = 6)
      ),
      data = 12
    )

    // Remove 0s
    val head = removeZeros(root)
    printTree(head)
  }

  private def printTree(node: Option[TreeNode]): Unit = {
    node match {
      case Some(x) => printTree(x)
      case _ => println("Empty")
    }
  }

  private def printTree(node: TreeNode): Unit = {
    println(node.data)
    node.nodes.foreach(printTree)
  }


  def removeZeros(node: TreeNode): Option[TreeNode] = {
    if (node.data ==  0) None
    else {
        Some(
          TreeNode(
            data = node.data,
            nodes = node.nodes.map(removeZeros).filter(_.isDefined).map(_.get)
          )
        )
    }
  }

}
