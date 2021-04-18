package lspace.provider.transaction

import lspace.provider.mem.MemNode
import lspace.structure.Node

trait TNode extends MemNode with TResource[Node]
