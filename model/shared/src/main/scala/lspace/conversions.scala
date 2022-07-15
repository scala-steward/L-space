package lspace

given someConversion[a, b](using Conversion[a, b]): Conversion[Some[a], Option[b]] = optionA =>
  optionA.map[b](a => implicitly[Conversion[a, b]].apply(a))

given noneConversion[b]: Conversion[None.type, Option[b]] = identity
