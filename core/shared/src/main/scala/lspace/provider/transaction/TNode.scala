package lspace.provider.transaction

import lspace.provider.mem.MemNode
import lspace.structure.{Graph, Node}

trait TNode extends MemNode with TResource[Node]
