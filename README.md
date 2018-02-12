# Recursive Language

A language for writing recursive functions.

## Requirements

DrRacket (https://racket-lang.org/)

## Installation

1. Open DrRacket. Click `File` > `Install Package...`.
2. Type `https://github.com/sorawee/recursive-language.git` in the textbox. Click `Install`.

## How to run

In the editor, you can define recursive functions and call them.

Following are builtin:

1. `z` and `s` functions (zero constant and successor functions)
2. `id_<place>^<arity>` where `<place>` and `<arity>` are nats (identity / projection functions)
3. `const_<n>` where `<n>` is a nat
4. `Cn` (composition)
5. `Pr` (primitive recursion)
6. `Mn` (Minimization): NOT YET IMPLEMENTED -- WILL BE ADDED SOON

Note that the language uses S-Expression, so instead of writing `Pr[f, g]`, write `[Pr f g]` instead. Similarly, instead of writing `Cn[f, a, b, c]`, write `[Cn f a b c]`.

You can give a name to a recursive function using the `define` construct. For example:

```
[define sum [Pr id_1^1 [Cn s id_3^3]]]
```

defines `sum` to be the recursive function `[Pr id_1^1 [Cn s id_3^3]]`.

Then, we can call the function by:

```
(sum 10 23)
```


Click `Run` at the top right of the editor. This results in:

```
(sum 2 23): 25
```

in the output panel.

## Example

```
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
```

results in:

```
(sum 2 23): 25
(prod 1 1): 1
(prod 2 2): 4
(prod 3 3): 9
(fact 4): 24
```
