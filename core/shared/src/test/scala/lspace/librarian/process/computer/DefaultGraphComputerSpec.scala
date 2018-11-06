package lspace.librarian.process.computer

import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.Graph

class DefaultGraphComputerSpec extends GraphComputerSpec {
  implicit lazy val graph: Graph = MemGraph("DefaultGraphComputerSpec")
  val computer: GraphComputer    = DefaultStreamComputer()

}
