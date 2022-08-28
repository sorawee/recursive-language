#lang scribble/manual

@(require syntax/parse/define)

@(define-syntax-parse-rule (rcode x ...)
   (tt x ...))

@title{Recursive Language}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[recursive-language #:lang]


A language for writing @emph{recursively computable functions} (or simply @emph{recursive functions}),
which are functions in recursion theory that consume natural numbers and produce a natural number.
See Chapter 6 of @emph{Computability and Logic} (George Boolos, John P. Burgess, and Richard Jeffrey) for details.

@section{Language Features}

@subsection{Import}

To make it clear what constructs and functions are used as a basis to define recursive functions,
we need to explicitly @emph{import} them with the @rcode{import} statement:

@codeblock[#:keep-lang-line? #f]{
#lang recursive-language
import <construct-1>, <construct-2>, ..., <construct-n>;
}

Following are core constructs:

@itemlist[
  @item{@rcode{z} and @rcode{s} functions (zero constant function and successor function, with arity 1)}
  @item{@rcode{id_<place>^<arity>} where @rcode{<place>} and @rcode{<arity>}
        are natural numbers (identity/projection functions, with arity @rcode{<arity>})}
  @item{@rcode{Cn[<f>, <g-1>, ..., <g-m>]} (composition)}
  @item{@rcode{Pr[<f>, <g>]} (primitive recursion)}
  @item{@rcode{Mn[<f>]} (minimization)}
]

Furthermore, following derived functions (presented in the book) are also provided for convenience.

@itemlist[
  @item{@rcode{const_<n>} where @rcode{<n>} is a natural number (constant function, with arity 1)}
  @item{@rcode{sum} and @rcode{prod} (addition and multiplication, with arity 2)}
  @item{@rcode{pred} (predecessor function, with arity 1)}
  @item{@rcode{monus} (subtraction on natural numbers, with arity 2)}
  @item{@rcode{sgn} and @rcode{~sgn} (zero predicate and its negated form, with arity 1)}
]

@subsection{Definition}

We can define recursive functions using @rcode{=} where the LHS is an identifier name and
the RHS is an expression that builds a recursive function. For example:

@codeblock[#:keep-lang-line? #f]{
#lang recursive-language
sum = Pr[id_1^1, Cn[s, id_3^3]];
prod = Pr[z, Cn[sum, id_1^3, id_3^3]];
}

defines @rcode{sum} to be the (primitive) recursive function @rcode{Pr[id_1^1, Cn[s, id_3^3}]] and
@rcode{prod} to be the (primitive) recursive function @rcode{Pr[z, Cn[sum, id_1^3, id_3^3}]]

@subsection{Print}

We can print results from function invocation by using the @rcode{print} statement.

@codeblock[#:keep-lang-line? #f]{
#lang recursive-language
print sum(10, 23);
}

Running the program will result in @rcode{33}.

@subsection{Check}

We can use the check construct to test that our recursive functions
are correct on given inputs. For example:

@codeblock[#:keep-lang-line? #f]{
#lang recursive-language
check sum(10, 23) = 33;
}

checks that @rcode{sum(10, 23)} should have the result @rcode{33}.

@subsection{Comment}

Comment can be written by using @code[#:lang "recursive-language"]{# ...}.

@section{Example}

@codeblock{
#lang recursive-language

import Pr, Cn, s, id, z, const;

sum = Pr[id_1^1, Cn[s, id_3^3]];
print sum(2, 23); # should be 25

prod = Pr[z, Cn[sum, id_1^3, id_3^3]];
print prod(3, 3); # should be 9

fact =
  Cn[Pr[const_1, Cn[prod, Cn[s, id_2^3], id_3^3]],
     id_1^1, id_1^1];

print fact(fact(3)); # should be 720
print fact(Cn[s, s](4)); # should be 720

check fact(3) = 6;
}
