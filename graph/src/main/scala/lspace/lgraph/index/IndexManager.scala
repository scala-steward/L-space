package lspace.lgraph.index

import lspace.lgraph.LGraph

abstract class IndexManager[G <: LGraph](val graph: G) {}
