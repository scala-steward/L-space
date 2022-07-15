package lspace

open class Node extends Resource[Node]

case class OrphanNode[IN, OUT](in: Set[IN], out: Set[OUT]) extends Node
