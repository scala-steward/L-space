package lspace

extension [predicate <: P[_]](predicate: predicate)
  infix def &&[predicate2 <: P[_]](predicate2: predicate2): And[And.AndTuple[(predicate, predicate2)]] = And(
    (predicate, predicate2)
  )
  infix def ||[predicate2 <: P[_]](predicate2: predicate2): Or[Or.OrTuple[(predicate, predicate2)]] = Or(
    (predicate, predicate2)
  )