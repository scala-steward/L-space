package lspace.lgraph

import lspace.structure.{Edge, Property, Resource}

object LEdge {}

trait LEdge[S, E] extends LResource[Edge[S, E]] with Edge[S, E]
