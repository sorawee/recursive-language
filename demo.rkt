#lang recursive-language

[define sum [Pr id_1^1 [Cn s id_3^3]]]

(sum 2 23)

[define prod [Pr z [Cn sum id_1^3 id_3^3]]]

(prod 1 1)
(prod 2 2)
(prod 3 3)

[define fact
  [Cn [Pr const_1 [Cn prod [Cn s id_2^3] id_3^3]] id_1^1 id_1^1]]

(fact 4)