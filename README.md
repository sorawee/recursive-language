# Recursive Language

A language for writing recursive functions. See Chapter 6, _Computability and Logic_ (George Boolos, John P. Burgess, and Richard Jeffrey) for details.

## Requirements

DrRacket (which can be downloaded at https://racket-lang.org/)

## Installation

1. Open DrRacket. Click `File` > `Install Package...`.
2. Type `https://github.com/sorawee/recursive-language.git` in the textbox. Click `Install`.

## Update

1. Click `File` > `Package Manager...`
2. Go to the `Currently Installed` tab.
3. Click the row `recursive-language`, then click `Update`.

## How to run

### Import

To make it clear what constructs you use to define functions, you need to
explicitly _import_ constructs that you wish to use with the `import` statement:

`import <construct_1>, <construct_1>, ..., <construct_n>;`

Following are core constructs that you can used to define a function.

1. `z` and `s` functions (zero const function and successor function)
2. `id_<place>^<arity>` where `<place>` and `<arity>` are nats (identity / projection functions)
3. `Cn` (composition)
4. `Pr` (primitive recursion)
5. `Mn` (minimization)

Furthermore, following are builtin functions that could be constructed by
core constructs. They are provided for convenience.

0. `const_<n>` where `<n>` is a nat (const function)
1. `sum` and `prod` (addition and multiplication)
2. `pred` (`pred(x) = max(0, x - 1)`)
3. `monus` (`monus(a, b) = max(0, a - b)`)
4. `sgn` and `~sgn` (`sgn(0) = 0`, `sgn(x') = 1`, `~sgn(x) = 1 - sgn(x)`)

### Definition

In the editor, you can define recursive functions using `=` where the LHS is
an identifier name and the RHS is a recursive function. For example:
.

```
sum = Pr[id_1^1, Cn[s, id_3^3]];
prod = Pr[z, Cn[sum, id_1^3, id_3^3]];
```

defines `sum` to be the recursive function `Pr[id_1^1, Cn[s, id_3^3]]` and
`prod` to be the recursive function `Pr[z, Cn[sum, id_1^3, id_3^3]]`

### Function Call

We can call the function by:

```
sum(10, 23);
prod(3, 4);
```

Click `Run` at the top right of the editor. This results in:

```
(sum 2 23): 25
(prod 3 4): 12
```

in the output panel (the expression before `:` is the call in S-Expression).

## Example

```
#lang recursive-language

import Pr, Cn, s, id, z, const;

sum = Pr[id_1^1, Cn[s, id_3^3]];

sum(2, 23);

prod = Pr[z, Cn[sum, id_1^3, id_3^3]];

prod(1, 1);
prod(2, 2);
prod(3, 3);

fact =
  Cn[Pr[const_1, Cn[prod, Cn[s, id_2^3], id_3^3]], id_1^1, id_1^1];

fact(4);
```

results in:

```
(sum 2 23): 25
(prod 1 1): 1
(prod 2 2): 4
(prod 3 3): 9
(fact 4): 24
```
