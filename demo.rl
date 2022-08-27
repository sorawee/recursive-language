#lang recursive-language

import Pr, Cn, s, id, z, const;

sum = Pr[id_1^1, Cn[s, id_3^3]];
check sum(2, 23) = 25;

prod = Pr[z, Cn[sum, id_1^3, id_3^3]];
check prod(3, 3) = 9;

fact =
  Cn[Pr[const_1, Cn[prod, Cn[s, id_2^3], id_3^3]],
     id_1^1, id_1^1];

check fact(fact(3)) = 720;

# FIXME! This test is incorrect.
check fact(Cn[s, s](4)) = 721;

f = s;
