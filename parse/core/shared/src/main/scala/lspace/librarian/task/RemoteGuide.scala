package lspace.librarian.task

//import lspace.librarian.traversal.Traversal
//import lspace.provider.remote.RemoteGraph
//import lspace.structure.{ClassType, Graph}
//import monix.reactive.Observable
//import shapeless.HList
//
//object RemoteGuide {
//  def apply: RemoteGuide[Observable] = new RemoteGuide[Observable]() {}
//}
//trait RemoteGuide[F[_]] extends Guide[F] {
//  def buildTraversal[Out](
//      traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]): RemoteGraph => F[Out] = { graph =>
//    }
//
//}
