#lang pollen

◊(define-meta title "Delimited Continuations with shift/reset in Elixir")
◊(define-meta date "2021-08-27")
◊(define-meta tags ("elixir" "delimited continuations" "proof of concept"))

◊ul{
◊li{◊hyperlink["#sec:introduction"]{Introduction}}
◊li{◊hyperlink["#sec:examples"]{Examples}}
◊li{◊hyperlink["#sec:limitations"]{Limitations}}
◊li{◊hyperlink["#sec:references-and-further-resources"]{References and further resources}}
◊li{◊hyperlink["#sec:appendix"]{Appendix}}
◊li{◊hyperlink["#sec:footnotes"]{Footnotes}}
}



◊h1[#:id "sec:introduction"]{Introduction}

◊p{Ever since I heard about those things called "continuations", I've
been fascinated by what they are and how they work, specially after
discovering this thing called ◊code{callCC}◊footnote{call-cc-wiki} ◊footnote{scheme-call-cc-wiki}. If this was not
hard enough to grasp, some time later I discovered that continuations
can be ◊emph{delimited} or ◊emph{undelimited}. Recently I found several
references about continuations◊footnote{continuation-study-group-gh}, but they tend to be papers that
are hard to read without some prior knowledge, which I don't
have. Therefore, I'm unable to present here some intuitive explanation
about them. Maybe after much more reading.}

◊p{Instead, this post is about a proof of concept (POC) delimited
continuations library in Elixir. This is the result of my attempts at
trying to understand (delimited) continuations by way of practice. The
resulting library converts Elixir code into continuation passing
style, and then uses that transformed code to implement the control
operators ◊code{shift} and ◊code{reset}. Theses things will be briefly discussed
here. I hope this may help someone else trying to grasp continuations
and, if I'm lucky, someone may teach me more by pointing out the gaps
in my understanding and implementation. =)}

◊p{This post presents some (hopefully) interesting examples of what can
be done using those operators. A very simplistic (and perhaps
erroneous, as I'm still grasping those concepts… beware!)
explanation of what I gathered so far about this topic and a few
implementation details I stumbled upon are presented in the
◊hyperlink["#sec:appendix"]{appendix}. The implementation is available on ◊hyperlink["https://github.com/thalesmg/campinas/tree/2d41f2697395c2fc542500ad32c62f0f307c7220"]{Github}.}

◊h1[#:id "sec:examples"]{Examples}

◊p{Here, I'll present the basic usage of the library and some
examples. For more on ◊hyperlink["#sec:cps"]{CPS} and ◊hyperlink["sec:shift---reset"]{◊code{shift/reset}}, see the corresponding
sections. Several examples will slowly build over one another because
I found it interesting to do so when I was trying to grasp the
concepts myself gradually.}

◊p{The experimental library is Campinas◊footnote{campinas-name}, available on ◊hyperlink["https://github.com/thalesmg/campinas/tree/2d41f2697395c2fc542500ad32c62f0f307c7220"]{Github}. To
use it, just add ◊code{use Campinas} at the beginning of your module,
define functions to be CPSed using the ◊code{defcps/2} macro and run them
using the ◊code{runCPS/1} macro. Notice that the current implementation is
pretty much a POC, so several AST nodes are not yet supported, such as
pipes, ◊code{for/1}, ◊code{receive/after} blocks and many others.}

◊p{The most basic example is one where nothing fancy is done:}

◊highlight['elixir]{
defmodule Example do
  use Campinas

  defcps ex1(x) do
    y = x + 1
    z = 2 * y - 3
    div(z, 3)
  end
end
}

◊p{Running ◊code{runCPS(Example.ex1(10))} gives ◊code{6}, as expected.}

◊p{Things start to get a bit more interesting when we introduce
◊code{shift/reset}. I'll give a few very basic examples with growing
complexity to try to demonstrate how those work intuitively. First,
using just ◊code{reset} basically does nothing by itself:}

◊highlight['elixir]{
defcps ex2() do
  x = reset do
        4
      end
  x + 1
end
}

◊p{◊code{runCPS(Example.ex2())} results in ◊code{5}, as if ◊code{reset} wasn't
there. Similarly, an expression using only ◊code{shift} appears to have no
visible effects as well (◊code{cont} is the name we give our continuation;
it can be something else as well):}

◊highlight['elixir]{
defcps ex3() do
  shift cont do
    6
  end
end
}

◊p{◊code{runCPS(Example.ex3())} indeed results in ◊code{6}. The first strange
example is one where we use ◊code{shift} in a more complex
expression. Let's take ◊code{ex1} and use ◊code{shift} in its second expression: ◊span[#:id "example_ex4"]{}}

◊highlight['elixir]{
defcps ex4(x) do
  y = x + 1
  z = 2 * shift cont do
            99
          end - 3
  div(z, 3)
end
}

◊p{If ◊code{shift} were truly a no-op, one would expect
◊code{runCPS(Example.ex4(10))} to yield ◊code{65}. Yet, the final result is
◊code{99}! It is as if invoking ◊code{shift} acts like an early function exit
such as ◊code{return} from a more imperative language. Notice that we
didn't invoke the continuation inside ◊code{shift}. If we do that:}

◊highlight['elixir]{
defcps ex5(x) do
  y = x + 1
  z = 2 * shift k do  # also changed the name, for fun
            k(99)     # here we invoke the captured continuation
          end - 3
  div(z, 3)
end
}

◊p{... then, indeed, ◊code{runCPS(Example.ex5(10))} yields ◊code{65}. But what if
we do more than simply invoking the continuation inside ◊code{shift}?}

◊highlight['elixir]{
defcps ex6(x) do
  y = x + 1
  z = 2 * shift k do
            k(99) - 60
          end - 3
  div(z, 3)
end
}

◊p{This results in ◊code{5}! In effect, it is the same as ◊code{ex4}, but with ◊code{60}
subtracted from it. Essentially, when we invoke ◊code{shift}, the whole
function result is ◊code{shift}'s body. In this example, the continuation
is a function that is roughly equivalent to ◊code{fn v -> div(2 * v - 3, 3)
end}.}

◊p{That behavior is changed when we add ◊code{reset} to the mix. It
effectively ◊emph{delimits} the extent to which ◊code{shift} can capture the
continuation.}

◊highlight['elixir]{
defcps ex7(x) do
  y = x + 1
  z = reset do
        2 * shift cont do
              cont(99) - 60
            end
      end - 3
  div(z, 3)
end
}

◊p{The result now is ◊code{45}. The captured continuation is now equivalent to
◊code{fn v -> 2 * v end} because of the enclosing ◊code{reset}. Another
interesting thing is that the continuation may be invoke more than
once. The following example results in ◊code{13}:}

◊highlight['elixir]{
defcps ex8() do
  z = reset do
        2 * shift cont do
              cont(cont(8)) + 11
            end
      end - 3
  div(z, 3)
end
}

◊p{Multiple CPSed functions that use ◊code{shift} and ◊code{reset} can be composed
using the ◊emph{ad-hoc} syntax ◊code{@[expression]}. We can break up ◊code{ex8} into
two smaller functions, and has the same behavior as before:}

◊highlight['elixir]{
defcps ex9() do
  shift cont do
    cont(cont(8)) + 11
  end
end

defcps ex10() do
  z = reset do
        2 * @[ex9()]
      end - 3
  div(z, 3)
end
}

◊p{We can also ◊code{shift} inside of a tuple literal:}

◊highlight['elixir]{
defcps ex11() do
  {
    1,
    2,
    shift cont do
      Tuple.to_list(cont(99))
    end,
    4
  }
end
}

◊p{This results in ◊code{[1, 2, 99, 4]}.}

◊p{A more bizarre example taken from one of the original papers that
introduce ◊code{shift/reset}◊footnote{abstracting-control-danvy-filinski} is one that writes seemingly direct code
to compute "non-deterministically" combinations of numbers that add up
to a given value. We need to define a few functions that do the magic:}

◊highlight['elixir]{
@doc "aborts the computation early"
defcps fail() do
  shift k do
    :no
  end
end

@doc "tries to continue with both true and false"
defcps flip() do
  shift k do
    k(true)
    k(false)
    @[fail()]
  end
end

@doc "continues with all numbers below a maximum"
defcps choice(n) do
  # by the way, `if` is supported
  if n < 1 do
    @[fail()]
  else
    if @[flip()] do
      @[choice(n - 1)]
    else
      n
    end
  end
end

@doc """
tries to find all x > y > z with x <= n such that x + y + z = s
"""
defcps triples(n, s) do
  x = @[choice(n)]
  y = @[choice(x - 1)]
  z = @[choice(y - 1)]

  if x + y + z == s do
    send(self(), {:found, {x, y, z}})
  else
    @[fail()]
  end
end
}

◊p{Then, to find such triples with ◊code{n = 9} and ◊code{s = 15}:}

◊highlight['elixir]{
defcps ex12() do
  @[triples(9, 15)]
end
}

◊p{Running this yields:}

◊highlight['elixir]{
iex(201)> runCPS(Example.ex12())
:no
iex(202)> flush()
{:found, {6, 5, 4}}
{:found, {7, 5, 3}}
{:found, {7, 6, 2}}
{:found, {8, 4, 3}}
{:found, {8, 5, 2}}
{:found, {8, 6, 1}}
{:found, {9, 4, 2}}
{:found, {9, 5, 1}}
:ok
iex(203)>
}

◊h2[#:id "sec:effect-systems"]{Effect systems}

◊p{As the final example, I'll show simple error and state effects built
upon those operators. It has been observed that delimited
continuations can be used to model effect systems◊footnote{ghc-continuation-primops} ◊footnote{algebraic-effects-handlers}.}

◊p{The simplest one is the error effect. Reminding ourselves of example
◊hyperlink["#example_ex4"]{◊code{ex4}}, an early exit would be implemented as simply as calling
◊code{shift}. To add extra spice, we'll consider ◊emph{recoverable} exceptions:
the user provides a handler that receives the error ◊code{e} and decides if
computation should halt and return ◊code{{:error, e}}, or if it should
continue (and provide a value back to the computation).}

◊highlight['elixir]{
# the program
defmodule ErrorExample do
  use Campinas
  alias Campinas.Effects.Error

  defcps program1(x) do
    y = x * x - 1

    if y < 0 do
      @[Error.error(:negative)]
    end

    result =
    if y > 100 do
      @[Error.error({:too_big, y})]
    else
      div(y, 2)
    end

    result - 1
  end
end

# the usage
handler = fn
  {:too_big, n} ->
    send(self(), {:big_number, n})

  if rem(n, 2) == 0 do
    {:cont, 0}
  else
    :halt
  end

  e ->
    send(self(), {:some_error, e})
  :halt
end

run_error(ErrorExample.program1(2), handler)
# should return `{:ok, 0}` without calling the handler

run_error(ErrorExample.program1(11), handler)
# should return `{:ok, -1}` and call the handler, which continues

run_error(ErrorExample.program1(0), handler)
# returns `{:error, :negative}` and call the handler, which aborts
}

◊p{The state effect is our last example. It has two operations: ◊code{get/0},
which simply reads the current state, and ◊code{set/1}, which defines a new
state. The stateful program is run by being fed to ◊code{run_state/2} along
with the initial state. This returns ◊code{{:ok, result, final_state}}.}

◊highlight['elixir]{
# the program
defmodule StateExample do
  use Campinas
  alias Campinas.Effects.State

  defcps program1(x) do
    s1 = @[State.get()]
    s2 = x + s1

    if rem(s2, 2) == 0 do
      @[State.set(s2 + 11)]
    else
      @[State.set(4 * s2)]
    end

    2 * s2 + 1
  end
end

# the usage
run_state(StateCases.program1(11), 20)
# returns `{:ok, 63, 124}`; 63 is the result; 124 is the final state

run_state(StateCases.program1(10), 20)
# returns `{:ok, 61, 41}`; 61 is the result; 41 is the final state
}

◊p{Notice that there is no mutation involved, nor exceptions being
raised/thrown (in the Elixir/Erlang sense) in those examples. ;)}

◊h1[#:id "sec:limitations"]{Limitations}

◊p{I have not implemented several AST node possibilities in the
transformation, so almost anything outside the examples in the tests
will probably not work. =)}

◊p{The example from the composable-continuation tutorial on the Scheme
Wiki does not work with the current version. I believe that
◊code{Enum.each} (the equivalent of ◊code{for-each} there) would need to be
CPSed for that to work.}

◊h1[#:id "sec:references-and-further-resources"]{References and further resources}

◊p{Here are some resources I have used, not necessarily in their
entirety, and others that I have found while researching this topic.}

◊ul{
◊li{◊hyperlink["https://www.youtube.com/watch?v=QNM-njddhIw"]{Delimited Continuations for Everyone by Kenichi Asai (Youtube)}

Nice video explaining delimited continuations with examples. It is
also where I found some recommendations of further resources (around
01:30).}
◊li{◊hyperlink["https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/abs/representing-control-a-study-of-the-cps-transformation/37193FD94F87443338FC7F519783FF0A"]{Olivier Danvy and Andre Filinski, "Representing Control: a Study of the CPS Transformation", MSCS, 1992}

The introductory paper recommended by Kenichi Asai. It does seem to
have some prior knowledge assumptions, but seems very comprehensive
and has very helpful tables of conversion rules for CPSing programs.}
◊li{◊hyperlink["https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.43.8753"]{Olivier Danvy and Andre Filinski, “Abstracting Control,” LISP and Functional Programming, 1990}

Another earlier paper by the authors who introduced ◊code{shift} and
◊code{reset}. It is more compact, has a couple examples, but is much more
dense and harder to understand (much more assumed knowledge about
concepts and notation).}
◊li{◊hyperlink["https://docs.racket-lang.org/reference/cont.html"]{Racket Reference Manual on Continuations}

Great source of references and displays other control operators. Not
quite didactic, but I recommend browsing it and trying out the
operators, since the implementation is solid in Racket.}
◊li{◊hyperlink["http://community.schemewiki.org/?composable-continuations-tutorial"]{Composable Continuations Tutorial on Scheme Wiki}

A nice and short tutorial with some examples that are very
illuminating examples that are valuable to be worked out manually.}
◊li{◊hyperlink["https://github.com/swannodette/delimc"]{◊code{delimc} by David Nolen}

A delimited continuations library for Clojure. Nice and short
implementation to study.}
◊li{◊hyperlink["https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst"]{The proposal to add delimited control primops to GHC} and ◊hyperlink["https://mail.haskell.org/pipermail/ghc-devs/2020-July/019016.html"]{a companion email thread} ◊span[#:id "ref_ghc_primops_alexis"]{}

Low level discussion of adding control operators similar to ◊code{shift}
and ◊code{reset} to GHC, and how these affect the execution stack.}
◊li{◊hyperlink["https://stackoverflow.com/a/29838823/2708711"]{This answer to a StackOverflow question about continuation prompts by Alexis King} ◊span[#:id "ref_stackoverflow_alexis"]{}

Has some nice visualizations relating stack frames and delimited
continuations.}
◊li{◊hyperlink["https://wingolog.org/archives/2010/02/26/guile-and-delimited-continuations"]{guile and delimited continuations, by Andy Wingo} ◊span[#:id "ref_guile_wingo"]{}

One of the implementers of GNU Guile (a Scheme implementation)
discusses adding delimited continuations to the language. Has some
nice illustrations of the stack for the ◊code{control/prompt} operators
(cousins of ◊code{shift/reset}).}
◊li{◊hyperlink["https://github.com/rain-1/continuations-study-group"]{rain-1's continuation study group}

A vast collection of papers and references about continuations in
general. It'll take quite a while to chew through all that. =)

I'd love to know if this group has a forum or similar channel where
one could ask questions.}
}

◊h1[#:id "sec:appendix"]{Appendix}

◊h2[#:id "sec:continuation-passing-style--cps-"]{Continuation Passing Style (CPS)}

◊p{This sections describes briefly what CPS is and some decisions that I
had to make in the implementation in order for it to work. Although
I'm still making sense of them 🙈.}

◊p{A continuation is the materialization of "what comes next" at a given
point of execution of a program. Or, a continuation is the evaluation
context surrounding the ◊emph{reducible expression}
(◊emph{redex})◊footnote{racket-cont-model}. Using the same example
from the Racket documentation:}

◊highlight['elixir]{
#  continuation
# ↓↓↓↓
  4 - (1 + 1)
#     ↑↑↑↑↑↑↑
#      redex
}

◊p{Here, in order for the whole expression to be reduced, the redex is
◊code{(1 + 1)}, and the continuation is ◊code{4 - _}, where ◊code{_} takes the place
of the redex as it is reduced. As another example:}

◊highlight['elixir]{
def some_fun() do
  x = 1      # the lines below are this expression's continuation.
  y = x + 2  # `x` is the "redex" that is fed here, and `_ + 3` is
             # this line's continuation.
  y + 3      # within this line, `_ + 3` is `y`'s continuation
end
}

◊p{◊emph{Continuation Passing Style (CPS)} is a way of writing functions and
expressions where the continuation is passed as an explicit argument
to the redex.}

◊h3[#:id "sec:irreducible-values"]{Irreducible values}

◊p{The simplest case is that of a value that cannot be reduced
further. Using the
notation◊footnote{representing-control-danvy-filinski} ◊code{[[ E
]]} to denote the CPS conversion of a term ◊code{E}, the conversion of
a pure value is simply:}

◊highlight['racket]{
[[ x ]] = λκ. κ x
}

◊p{In Elixir:}

◊highlight['elixir]{
# a simple value...
1
# ... in CPS form becomes:
fn k -> # `k` is the continuation, to be provided by some other code.
  k.(1) # that continuation is invoked and receives the value to
        # proceed.
end
}

◊h3[#:id "sec:primitive-function-application"]{Primitive function application}

◊p{Another simple case is that of primitive function application. A
primitive function is one that is considered a "black box" and its
definition cannot be directly converted into CPS. I considered local
and remote function calls as primitives.}

◊p{For a primitive function ◊code{p} applied to ◊code{x}, its conversion rule is:}

◊highlight['racket]{
[[ p x ]] = λκ. [[ x ]] (λa. κ (p a))
}

◊p{Let's take as an example the negate unary operator, ◊hyperlink["https://hexdocs.pm/elixir/Kernel.html#-/1"]{◊code{Kernel.-/1}}.}

◊highlight['elixir]{
- x
# ... in CPS form becomes:
fn k1 ->         # the outer continuation
  (fn k2 ->      # ─┐ this `k2` is the lambda defined below
    k2.(x)       #  │
  end).(fn a ->  # <┘
                 # the outer continuation receives the result of the
    k1.(- a)     # primitive function application
  end)
end
}

◊p{If you manually evaluate the above expression, you'll see that it is
indeed equivalent to the original expression.}

◊p{If there are multiple arguments, we first have to
◊emph{curry}◊footnote{currying-wiki} the function before
converting. This is the default behavior in a few languages such as
Haskell and OCaml, but is a bit unusual in Elixir. If we start with
the following for
◊hyperlink["https://hexdocs.pm/elixir/Kernel.html#-/2"]{◊code{Kernel.-/2}}:}

◊highlight['elixir]{
fn x, y ->
  x - y
end
}

◊p{The curried form (not yet "◊emph{CPSed}"), is equivalent to:}

◊highlight['elixir]{
fn x ->
  fn y ->
    x - y
  end
end
}

◊p{The rule for a 2-arity primitive application is:}

◊highlight['racket]{
[[ p x y ]] = λκ. [[ x ]] (λa. [[ y ]] (λb. κ (p a b)))
}

◊p{This rule can be extended further for more arguments. Expressing this
in Elixir: ◊span[#:id "cps_prim_app_minus1"]{}}

◊highlight['elixir]{
# assuming `x` and `y` are in scope here.
fn k1 ->
  (fn k2 ->
    k2.(x)
  end).(fn a ->
    (fn k3 ->
      k3.(y)
    end).(fn b ->
      k1.(a - b)
    end)
  end)
end
}

◊p{Pass this thing the "final continuation" (commonly the identity
function ◊hyperlink["https://hexdocs.pm/elixir/Function.html#identity/1"]{◊code{Function.identity/1}} or, more compactly, ◊code{& &1}), you should
see it results in ◊code{-1} as expected.}

◊p{A special case is that of 0-arity primitive functions. In that case,
we just invoke the function and pass it to the continuation, as if it
were a pure value.}

◊highlight['elixir]{
node()
# ... becomes simply:
fn k -> k.(node()) end
}

◊h3[#:id "sec:lambdas"]{Lambdas}

◊p{The next case to consider is how to convert a lambda definition into
CPS. To do so, we make it accept a continuation as the first argument,
then immediately apply it to a lambda that takes the "original"
argument. The body of this inner lambda is another lambda that takes
another continuation, with the "CPSed" (converted into CPS) version of
the original lambda body fed this inner continuation.}

◊highlight['racket]{
[[ λx. M ]] = λκ1. κ1 (λx. λκ2. [[ M ]] κ2)
}

◊p{Since that is quite convoluted, let's visualize it by considering the
identity function:}

◊highlight['elixir]{
fn x -> x end
}

◊p{In CPS, it becomes:}

◊highlight['elixir]{
fn k1 ->      # the outer continuation;
  k1.(fn x -> # the argument;
    fn k2 ->  # takes another continuation;
      # then we CPS the body of the original lambda and feed it k2.
      # [[ x ]] k2
    end
  end)
end
# ↓↓↓↓↓↓↓↓↓↓↓↓↓
fn k1 ->
  k1.(fn x ->
    fn k2 ->
      # since it is a irreducible value, we apply the same rules as
      # above.
      (fn k3 ->
        k3.(x)
      end).(k2)
    end
  end)
end
}

◊p{In the above example, one could
β-reduce◊footnote{lambda-reduction-wiki} the inner lambda and simplify
further. But I'll use this β-expanded version that generalizes better
for the cases below◊footnote{paper-doubts} .}

◊p{There is an additional detail about the rule above: if a lambda like
the above is directly applied in code, as in ◊code{(fn x -> x end).(1)},
then the above conversion is the one use as the CPSed lambda to be
applied to ◊code{1} (as will be explored later). But if this lambda is
returned as a value, one must wrap it in another continuation layer as
if it were a pure value:}

◊highlight['elixir]{
# the final version of our example, when returned as a value
fn k0 ->        # ← notice the extra continuation `k0`
  k0.(fn k1 ->  # ←
    k1.(fn x ->
      fn k2 ->
        (fn k3 ->
          k3.(x)
        end).(k2)
      end
    end)
  end)
end
}

◊p{I probably have messed something thing up when implementing, but I
needed to do this in order for all the thing to behave as
expected. I'm curious to know the correct version of this. =)}

◊p{In case of multiple arguments, we curry the function as in the
primitive function application case above before CPSing it with
similar rules. As an example that mixes lambda definitions and
primitive function applications in its body. The big highlighted area
is the CPSed version of the "minus one" shown ◊hyperlink["#cps_prim_app_minus1"]{above}.}

◊highlight['elixir]{
# [[ fn x, y -> x - y end ]]
#
fn k0 ->                        # ← that extra continuation layer
  k0.(fn k1 ->                  # ─┐
    k1.(fn x ->                 # ─┘ stuff for the `x` argument
      fn k3 ->
        k3.(fn y ->
          fn k4 ->
            (fn k1 ->           # ─┐
              (fn k2 ->         #  │
                k2.(x)          #  │
              end).(fn a ->     #  │ this is the CPSed version of
                (fn k3 ->       #  │ the "minus one" function
                  k3.(y)        #  │ shown above...
                end).(fn b ->   #  │
                  k1.(a - b)    #  │
                end)            #  │
              end)              #  │
            end).(k4)           # ─┘ ... applied to the inner continuation
          end                   #    from the lambda
        end)
      end
    end)
  end)
end
}

◊p{This is already quite unwieldy, and anything more complicated tend to
grow quite fast in complexity. A good exercise is to take these more
basic examples and try to β-reduce them manually to get more intuition
of what-flows-where.}

◊p{We give the 0-arity case a slightly different treatment: we transform
the lambda body, wrap the result in a 0-arity lambda and return that
to a continuation.}

◊highlight['elixir]{
fn -> :result end
# ... becomes:
fn k0 ->             # again, extra continuation layer
  k0.(fn k1 ->
    k1.(fn ->        # notice that there is no argument here
      fn k2 ->       # ─┐
        k2.(:result) #  │ lambda body converted
      end            # ─┘
    end)
  end)
end
}

◊h3[#:id "sec:function-application"]{Function application}

◊p{The last type of terms I'll attempt to show here is the application of
functions to values. The implementation differentiates 3 sub-cases: i)
application of values to a lambda literal; ii) application to a named
lambda; iii) primitive function application. The last case was already
covered ◊hyperlink["#sec:primitive-function-application"]{above}, and it is things of the form ◊code{fun(x)} and
◊code{Node.ping()}. Case (i) is special because we use the converted lambda
version without the extra continuation layer. Finally, case (ii) is
treated specially because we assume that such function has already
been curried and CPSed, so we do not convert it further and simply
apply it using the rules that will be shown below.}

◊p{The conversion rule for an application is:}

◊highlight['racket]{
[[ M N ]] = λκ. [[ M ]] (λm. [[ N ]] (λn. m n κ))
}

◊p{As a final example, we consider the application to a named lambda.}

◊highlight['elixir]{
some_fun.(1)
# ... becomes
fn k1 ->
  some_fun.(fn m ->   # ← `some_fun` is considered already CPSed
    (fn k2 ->
      k2.(1)          # ← CPSed argument
    end).(fn n ->
      m.(n).(k1)
    end)
  end)
end
}

◊h3[#:id "sec:other-details"]{Other details}

◊p{For more details, I'll refer the reader to the implementation and to
some papers that describe the transformation ◊footnote{gh-lib-commit} ◊footnote{representing-control-danvy-filinski} ◊footnote{abstracting-control-danvy-filinski}.}

◊h2[#:id "sec:shift---reset"]{Shift / Reset}

◊p{Ok, that was quite a lot... Why go through all this trouble?}

◊p{The answer is that such transformations allow us to use some control
operators that are quite powerful. Some examples of applications that
can be implemented as libraries are: exceptions, backtracking search,
threads, generators and coroutines ◊footnote{matt-might-continuations}.}

◊p{Two of those operators are ◊code{shift} and ◊code{reset}, and there are a few
other more or less equivalent ones ◊footnote{racket-cont-doc}. They are most succinctly
conceptually described in the Racket documentation by the reduction
rules:}

◊highlight['racket]{
;; "=>" means "reduces to"
(reset val) => val
(reset E[(shift k expr)]) => (reset (λ (k) expr)
                                    (λ (v) (reset E[v])))
;; where `E` has no `reset`
}

◊p{I have not found in that documentation what ◊code{E[_]} means. But, by
experimenting with the operators, it looks like it means the ◊emph{dynamic
continuation enclosing} the call to ◊code{shift}, up to but not including
◊code{reset}. So, in:}

◊highlight['racket]{
(+ 1
   (reset
    (* 2
       (shift c
         (* 3 (c 4))))))
}

◊p{... ◊code{E[_]} means ◊code{(λ (v) (* 2 v))}. Indeed, the expression above
evaluates to ◊code{25}. Using the second evaluation rule:}

◊highlight['racket]{
(+ 1
   (reset
    ((λ (k) (* 3 (k 4)))
     (λ (v) (* 2 v)))))
;; β-reducing...
(+ 1
   (reset
    (* 3 ((λ (v) (* 2 v)) 4))))
;; using the 1st reduction rule for `reset`...
(+ 1 (* 3 ((λ (v) (* 2 v)) 4)))
;; β-reducing...
(+ 1 (* 3 (* 2 4)))
;; which yields 25
}

◊p{So, shortly, ◊code{shift} captures the continuation and binds it to be used
possibly multiple times. The extent of what is capture is determined
by the presence of ◊code{reset}, which acts as a delimiter (hence the name
◊emph{delimited continuations}).}

◊p{Just to illustrate, the example above could be written in Elixir as:}

◊highlight['elixir]{
x = reset do
      2 * shift cont do
            3 * cont(4)
          end
    end
1 + x
}

◊p{I used an intermediate variable just to emphasize that the "remaining
lines" after an expression are continuations for it.}

◊p{I'll try to borrow the visualization ideas ◊hyperlink["#ref_ghc_primops_alexis"]{from} ◊hyperlink["#ref_guile_wingo"]{those} ◊hyperlink["#ref_stackoverflow_alexis"]{references} and
attempt to illustrate conceptually how these operators are working in
this example (it is almost certainly wrong concretely, I don't know
how Elixir/Erlang break up stack frames). In the images below, each
rectangle is a continuation frame, and ● are the places where the
redexes go into after being reduced.}

◊figure{◊img[#:class "db w-60 center" #:src "../images/elixir-delimited-continuations-stack1.png" #:id "stack1"]{}}

◊p{When ◊code{shift} is invoked, it essentially captures the frames from the
current one up to the nearest enclosing ◊code{reset}, packages those up in
◊code{cont}, and replaces them with its own body frames.}

◊figure{◊img[#:class "db w-60 center" #:src "../images/elixir-delimited-continuations-stack2.png" #:id "stack2"]{}}

◊p{Which reduces to ◊code{25}, as before.}

◊h1[#:id "sec:footnotes"]{Footnotes}

◊hr{}

◊ol{
◊deffn['call-cc-wiki]{
◊hyperlink["https://en.m.wikibooks.org/wiki/Haskell/Continuation_passing_style#callCC"]{https://en.m.wikibooks.org/wiki/Haskell/Continuation_passing_style#callCC}
}

◊deffn['scheme-call-cc-wiki]{
◊hyperlink["http://community.schemewiki.org/?call-with-current-continuation"]{http://community.schemewiki.org/?call-with-current-continuation}
}

◊deffn['continuation-study-group-gh]{
◊hyperlink["https://github.com/rain-1/continuations-study-group"]{https://github.com/rain-1/continuations-study-group}
}

◊deffn['campinas-name]{
It is the name of a ◊hyperlink["https://en.wikipedia.org/wiki/Campinas"]{city} whose contraction is ◊emph{CPS}.
}

◊deffn['abstracting-control-danvy-filinski]{
◊hyperlink["https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.43.8753"]{Olivier Danvy and Andre Filinski, “Abstracting Control,” LISP and Functional Programming, 1990}
}

◊deffn['ghc-continuation-primops]{
◊hyperlink["https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst"]{https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst}
}

◊deffn['algebraic-effects-handlers]{
◊hyperlink["https://doi.org/10.1016/j.entcs.2015.12.003"]{https://doi.org/10.1016/j.entcs.2015.12.003}
}

◊deffn['racket-cont-model]{
◊hyperlink["https://docs.racket-lang.org/reference/eval-model.html#%28part._cont-model%29"]{https://docs.racket-lang.org/reference/eval-model.html#%28part._cont-model%29}
}

◊deffn['representing-control-danvy-filinski]{
◊hyperlink["https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/abs/representing-control-a-study-of-the-cps-transformation/37193FD94F87443338FC7F519783FF0A"]{Olivier Danvy and Andre Filinski, "Representing Control: a Study of the CPS Transformation", MSCS, 1992}
}

◊deffn['currying-wiki]{
◊hyperlink["https://en.wikipedia.org/wiki/Currying"]{https://en.wikipedia.org/wiki/Currying}
}

◊deffn['lambda-reduction-wiki]{
◊hyperlink["https://en.wikipedia.org/wiki/Lambda_calculus#Reduction"]{https://en.wikipedia.org/wiki/Lambda_calculus#Reduction}
}

◊deffn['paper-doubts]{
It is also one point that I could
not understand quite well when reading the papers. I needed to do this
for the implementation to work properly for my test cases, but the
equations in ◊reference-footnote{representing-control-danvy-filinski} are somewhat different.
}

◊deffn['gh-lib-commit]{
◊hyperlink["https://github.com/thalesmg/campinas/tree/2d41f2697395c2fc542500ad32c62f0f307c7220"]{https://github.com/thalesmg/campinas/tree/2d41f2697395c2fc542500ad32c62f0f307c7220}
}

◊deffn['matt-might-continuations]{
◊hyperlink["https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/"]{"Continuations by example: Exceptions, time-traveling search, generators, threads, and coroutines", by Matt Might}
}

◊deffn['racket-cont-doc]{
◊hyperlink["https://docs.racket-lang.org/reference/cont.html"]{https://docs.racket-lang.org/reference/cont.html}
}
}
