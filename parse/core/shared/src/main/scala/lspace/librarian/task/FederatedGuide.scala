package lspace.librarian.task

import lspace.librarian.logic.Assistent

object FederatedGuide {
  def apply()(implicit _assistent: Assistent): FederatedGuide = new FederatedGuide {
    val assistent: Assistent = _assistent
  }
}
trait FederatedGuide extends AsyncGuide {

  //TODO: add graph-step
//  def executeRemote()
}
