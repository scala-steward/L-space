package lspace.librarian.provider.transaction

import lspace.librarian.provider.mem.MemNode
import lspace.librarian.structure.{Graph, Node}

trait TNode extends MemNode with TResource[Node]
