---
title: Wolf, Goat and Cabbage puzzle, in Coq
tags: coq, puzzles, formal methods,
alectryon: []
---

<link rel="stylesheet" href="/css/alectryon.css" type="text/css" />
<link rel="stylesheet" href="/css/tango_subtle.min.css" type="text/css" />

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Introduction](#introduction)
- [Basic definitions](#basic-definitions)
- [First attempt (problematic!)](#first-attempt-problematic)
- [Second attempt](#second-attempt)
- [Conclusions and Issues](#conclusions-and-issues)

<!-- markdown-toc end -->

# Introduction

Earlier this year, I attended the [Formal Methods for the Informal
Engineer](https://fmie2021.github.io/) workshop, and found that quite
fascinating. It is a huge field, and the topic is quite deep and
dense, as one would expect. It was my first introduction to the
[interactive theorem prover Coq](https://coq.inria.fr/), during the
[nice tutorial](https://youtu.be/5e7UdWzITyQ) by Cody Roux.

Some time later, after working thought the [Logical
Foundations](https://softwarefoundations.cis.upenn.edu/lf-current/index.html)
book from the Software Foundations series, I wanted to try and
practice it on my own on some toy problem. The ["wolf, goat and
cabbage
problem"](https://en.wikipedia.org/wiki/Wolf,_goat_and_cabbage_problem)
came to mind, and I have previously seems examples that show that the
[Alloy
Analyzer](https://alloytools.org/tutorials/online/frame-RC-1.html) and
[Z3](https://sudonull.com/post/29525-Formal-verification-on-the-example-of-the-wolf-goat-and-cabbage-problem)
could be used to solve it automatically. But I could not find an
equivalent solution in Coq. So I tried to see if I could do it with my
current knowledge.

In my first attempt, I managed to model and reproduce a known solution
as a proof! Or so I thought... The modeling had a problem which
allowed for a "single step" nonsense solution. That was uncovered
after I tried to use Coq's proof automation to see if it worked. Then
it showed me the bogus "solution", and I had to rethink how to
represent the problem.

The problem was that I was not constraining the contents of both river
banks correctly during the crossing, so it was possible that a bank
would suddenly change composition after a step. After constraining
those values more, I could reproduce the known solution, but lost the
ability to use the standard automation machinery... I guess I'd need
to create my own version of the `auto` or `eauto` tactics, like [Adam
Chlipala's `crush`](http://adam.chlipala.net/cpdt/), but I don't know
how to do that quite yet.

Even with this second solution, there some issues with it and I'm
still not sure if it is correct, since the first time and it was not
obvious to me that I had messed up. I'll share both solutions below,
and maybe one day someone can give me some hints on how to fix and
correct those issues.

I should also say that I used
[Alectryon](https://github.com/cpitclaudel/alectryon/) by Clément
Pit-Claudel to produce the Coq snippets with proof states below, and
it is quite magical. That, in turn, was taken from Philip Zucker's
[post](https://www.philipzucker.com/translating-z3-to-coq/) about
translating his Z3 tutorial from the FMIE workshop to Coq. If you
hover your cursor over the proof lines, it should show the hypothesis
and the goals at that point.

# Basic definitions

The definition below did not change between attempts, and were shared
by both solutions.

First, we need to import some modules so we can use a notation for
list literals. This is an amazing feature of Coq: the ability to
define custom notation.

```alectryon
From Coq Require Import Lists.List. Import ListNotations.
```

Then, we define the "objects" that the farmer wants to take from one
river bank to the other. Maybe it's strange to call a wolf and a goat
objects, but I couldn't think of another name for the type, and I went
with the nomenclature from the Alloy tutorial.

```alectryon
Inductive Object :=
| Wolf
| Goat
| Cabbage.
```

In each step, the farmer must cross from one side of the river to the
other alone or carrying one of the objects. I modeled the state of the
problem by defining which is the river bank the farmer will go to
next: either the left or the right side of the river.

```alectryon
Inductive Direction :=
| Right
| Left.
```

But some pairs of objects cannot be left unattended, else they eat
each other. Those pairs are "forbidden" when crossing the river. The
`eats` function tells us if either of two objects eats the other, and
the `NoEats` predicate uses that to assert that list of objects,
representing one side of the river, contains no forbidden pairs of
objects.

```alectryon
Definition eats (o1 o2 : Object) :  bool :=
  match (o1, o2) with
  | (Wolf, Goat) => true
  | (Goat, Wolf) => true
  | (Goat, Cabbage) => true
  | (Cabbage, Goat) => true
  | _ => false
  end.

Inductive NoEats : list Object -> Prop :=
| NoEatsNil : NoEats []
| NoEatsOne : forall o, NoEats [o]
| NoEatsMore : forall o os,
    NoEats os -> forallb (eats o) os = false -> NoEats (o :: os).
```

# First attempt (problematic!)

```alectryon
Module FirstAttempt.
```

The `RiverCrossing` predicate represents the state of the problem. It
is parameterized by the direction the farmer is going next, the
contents of the left and the right river banks. Then comes the
constructors of this predicate.

We have the initial state, where all objects are on the left bank, the
right bank is empty, and the farmer is going right. I used the [`In`
predicate](https://coq.inria.fr/library/Coq.Lists.List.html#In) to
avoid having to deal with specific list orderings.

Then the farmer can go right or left, alone. In that case, the
contents of both banks do not change, but we must enforce that the
objects that were just left alone do not eat each other.

Else, the farmer takes one object along the trip. I just checked that
the object being carried exists on the source bank in the initial
state and disappears from there in the final state, and that the
remaining objects don't eat each other. This has the problem I alluded
before: the constraints on the initial and final compositions of each
river bank are too loose, allowing for "quantum jumps".

```alectryon
Inductive RiverCrossing :
  Direction -> list Object -> list Object -> Prop :=
| initial : forall l,
    In Wolf l ->
    In Goat l ->
    In Cabbage l ->
    RiverCrossing Right l []
| goRightAlone :
    forall l r,
      NoEats l -> RiverCrossing Right l r ->
      RiverCrossing Left l r
| goLeftAlone :
    forall l r,
      NoEats r -> RiverCrossing Left l r ->
      RiverCrossing Right l r
| goRight :
    forall o l r l' r',
      In o l -> ~ (In o l') -> In o r' -> NoEats l' ->
      RiverCrossing Right l r ->
      RiverCrossing Left l' r'
| goLeft :
    forall o l r l' r',
      In o r -> ~ (In o r') -> In o l' -> NoEats r' ->
      RiverCrossing Left l r ->
      RiverCrossing Right  l' r'.
```

For reference, I'll just reproduce the proof using this model below,
but beware that it is wrong, as said before. Also, another thing of
notice is that the steps are "backwards": we start from the final
state (everything in the right bank) and work our way towards the
initial state, which should be the only way to produce a
`RiverCrossing` out of thin air.

```alectryon
Theorem solution : forall r,
    In Wolf r ->
    In Goat r ->
    In Cabbage r ->
    RiverCrossing Left [] r.
Proof.
  intros r Hw Hg Hc.
  (* step 7 : [G] -G> [W; C] => [] || [G; W; C] *)
  apply goRight with (o := Goat) (l := [Goat]) (r := [Cabbage; Wolf]).
  (* Goat in initial left bank *)
  apply in_eq.
  (* Goat not in final left bank *)
  apply in_nil.
  (* Goat in final right bank *)
  assumption.
  (* final left bank is empty, so it's safe *)
  apply NoEatsNil.

  (* step 6 : [G] <- [W; C] => [G] || [W; C] *)
  apply goLeftAlone.
  (* proof that Wolf doesn't eat Cabbage *)
  apply NoEatsMore. apply NoEatsOne. trivial.

  (* step 5 : [C; G] -C> [W] => [G] || [W; C] *)
  apply goRight with (o := Cabbage) (l := [Cabbage; Goat]) (r := [Wolf]).
  (* Cabbage in initial left bank *)
  apply in_eq.
  (* Cabbage not in final left bank *)
  intros contra; inversion contra; inversion H.
  (* Cabbage in final right bank *)
  apply in_eq.
  (* final left bank just has the Goat, so it's safe *)
  apply NoEatsOne.

  (* step 4 : [C] <G- [W; G] => [C; G] || [W] *)
  apply goLeft with (o := Goat) (l := [Cabbage]) (r := [Wolf; Goat]).
  (* Goat in initial right bank *)
  simpl; right; left; reflexivity.
  (* Goat not in final right bank *)
  intros contra; inversion contra; inversion H.
  (* Goat in final Left bank *)
  simpl; right; left; reflexivity.
  (* final right bank just has the Wolf, so it's safe *)
  apply NoEatsOne.

  (* step 3 : [W; C] -W> [G] => [C] || [W; G] *)
  apply goRight with (o := Wolf) (l := [Wolf; Cabbage]) (r := [Goat]).
  (* Wolf in initial left bank *)
  apply in_eq.
  (* Wolf not in final left bank *)
  intros contra; inversion contra; inversion H.
  (* Wolf in final right bank *)
  apply in_eq.
  (* final left bank just has the Cabbage, so it's safe *)
  apply NoEatsOne.

  (* step 2 : [W; C] <- [G] => [W; C] || [G] *)
  apply goLeftAlone.
  (* proof that Goat doesn't eat itself *)
  apply NoEatsOne.

  (* step 1 : [W; G; C] -G> [] => [W; C] || [G] *)
  apply goRight with (o := Goat) (l := [Wolf; Goat; Cabbage]) (r := []).
  (* Goat in initial left bank *)
  simpl; right; left; reflexivity.
  (* Goat not in final left bank *)
  intros contra; inversion contra; inversion H; inversion H0.
  (* Goat in final right bank *)
  apply in_eq.
  (* proof that Wolf doesn't eat Cabbage *)
  apply NoEatsMore. apply NoEatsOne. trivial.

  (* initial state: everyone on the left bank *)
  apply initial.
  (* Wolf on the initial left bank *)
  simpl; left; reflexivity.
  (* Goat on the initial left bank *)
  simpl; right; left; reflexivity.
  (* Cabbage on the initial left bank *)
  simpl; right; right; left; reflexivity.
Qed.
```

I was quite satisfied that this worked out, but luckily that model was
amenable to simple proof automation. Using the [`info_eauto`
tactic](https://coq.inria.fr/refman/proofs/automatic-tactics/auto.html#coq:tacn.info_eauto),
it proved that the objects could go all to the right side... But by a
strange path...

I also needed to add the constructors and functions relevant to the
problem in a hint database called `local` so that `eauto` could use
them.

```alectryon
Hint Constructors Object : local.
Hint Constructors Direction : local.
Hint Constructors NoEats : local.
Hint Resolve eats : local.
Hint Unfold eats : local.
Hint Constructors RiverCrossing : local.
Hint Unfold In : local.
Hint Resolve in_nil : local.
Hint Resolve in_eq : local.
Hint Resolve ex_intro : local.

Theorem solution_auto : forall r,
    In Wolf r ->
    In Goat r ->
    In Cabbage r ->
    RiverCrossing Left [] r.
Proof.
  info_eauto with local.
Qed.

Theorem bogus_solution : forall r,
    In Wolf r ->
    In Goat r ->
    In Cabbage r ->
    RiverCrossing Left [] r.
Proof.
  (* found by eauto *)
  intro.
  intro.
  intro.
  intro.
  simple eapply goRight.
  exact H1.
  simple apply in_nil.
  exact H1.
  exact NoEatsNil.
  (* oh no *)
  simple apply initial.
  exact H.
  exact H0.
  exact H1.
Qed.

End FirstAttempt.
```

# Second attempt

```alectryon
Module SecondAttempt.
```

In my second attempt, I only changed the `goRight` and `goLeft`
constructors to tighten up the restrictions on the contents of both
river banks. Anything on the opposing side in the initial state should
still be present in the final state plus the object that just
crossed. The initial side should contain a special object, the one
being carried, and all objects in that side should be present there in
the final state, except the carried object. This was the essential
change.

```alectryon
Inductive RiverCrossing :
  Direction -> list Object -> list Object -> Prop :=
| initial : forall l,
    In Wolf l ->
    In Goat l ->
    In Cabbage l ->
    RiverCrossing Right l []
| goRightAlone :
    forall l r,
      NoEats l -> RiverCrossing Right l r ->
      RiverCrossing Left l r
| goLeftAlone :
    forall l r,
      NoEats r -> RiverCrossing Left l r ->
      RiverCrossing Right l r
| goRight :
    forall l r l' r',
      (exists o,
          In o l /\ ~ (In o r)
          /\ (forall ol,
                 In ol l ->
                 (ol = o /\ In ol r') \/ (ol <> o /\ In ol l' /\ ~(In ol r')))
          /\ (forall or,
                 (In or r -> (or <> o /\ In or r')))) ->
      NoEats l' ->
      RiverCrossing Right l r ->
      RiverCrossing Left l' r'
| goLeft :
    forall l r l' r',
      (exists o,
          In o r /\ ~ (In o l)
          /\ (forall or,
                 In or r ->
                 (or = o /\ In or l') \/ (or <> o /\ In or r' /\ ~(In or l')))
          /\ (forall ol,
                 (In ol l -> (ol <> o /\ In ol l')))) ->
      NoEats r' ->
      RiverCrossing Left l r ->
      RiverCrossing Right l' r'.

Hint Constructors Object : local.
Hint Constructors Direction : local.
Hint Constructors NoEats : local.
Hint Resolve eats : local.
Hint Unfold eats : local.
Hint Constructors RiverCrossing : local.
Hint Unfold In : local.
Hint Resolve in_nil : local.
Hint Resolve in_eq : local.
Hint Resolve ex_intro : local.
```

Unfortunately, with those changes, neither `auto` nor `eauto` work
anymore. There are some goals that are repeated during the proof,
especially regarding goals and hypothesis of the forms `In X Xs` and
`~ (In X Xs)`. Using some excerpts from the
[CPDT](http://adam.chlipala.net/cpdt/) and
[PLF](https://softwarefoundations.cis.upenn.edu/plf-current/index.html)
books, I managed to make some rudimentary automated tactics for those
cases.

```alectryon
Ltac auto_not_in :=
  repeat match goal with
  | |- ~ (In ?X ?Xs) =>
    intros contra;
    repeat (destruct contra as [contra | contra];
            try discriminate;
            try apply contra);
    try destruct contra
  end.

Ltac not_in_absurd H :=
  match type of H with
  | In _ [] => exfalso; apply in_nil in H; apply H
  | In _ _ =>
    let H0 := fresh "H0" in
    try (inversion H as [H0 | H0]; try discriminate; not_in_absurd H0)
  end.

(* taken from:
 https://softwarefoundations.cis.upenn.edu/plf-current/LibTactics.html#lab524
 *)
Ltac jauto_set_hyps :=
  repeat match goal with H: ?T |- _ =>
    match T with
    | _ /\ _ => destruct H
    | exists a, _ => destruct H
    | _ => generalize H; clear H
    end
  end.

Ltac jauto_set_goal :=
  repeat match goal with
  | |- exists a, _ => esplit
  | |- _ /\ _ => split
  end.

Ltac jauto_set :=
  intros; jauto_set_hyps;
  intros; jauto_set_goal;
  unfold not in *.

Tactic Notation "jauto" :=
  try solve [ jauto_set; eauto with local ]; try auto_not_in.
Tactic Notation "jauto_fast" :=
  try solve [ auto with local | eauto with local | jauto ].
```

Armed with that, I redid my solution from before. Alas, I'm not able
at the moment to check if there are bogus solutions as I did before...
Again, I'll just reproduce the solution below.

```alectryon
Theorem solution : forall r,
    In Wolf r ->
    In Goat r ->
    In Cabbage r ->
    RiverCrossing Left [] r.
Proof.
  intros r Hw Hg Hc.

  (* step 7 : [G] -G> [W; C] => [] || [G; W; C] *)
  apply goRight with (l := [Goat]) (r := [Wolf; Cabbage]).
  exists Goat.
  try repeat (split; jauto).
  destruct ol; intros H; not_in_absurd H.
  left. split. trivial. assumption.
  destruct or; try discriminate; not_in_absurd H.
  destruct or; try discriminate; not_in_absurd H.
  assumption. assumption. constructor.

  (* step 6 : [G] <- [W; C] => [G] || [W; C] *)
  apply goLeftAlone.
  auto with local.

  (* step 5 : [C; G] -C> [W] => [G] || [W; C] *)
  apply goRight with (l := [Goat; Cabbage]) (r := [Wolf]).
  exists Cabbage.
  try repeat (split; jauto).
  destruct ol; intros H; not_in_absurd H.
  right. split. discriminate. split. auto with local. auto_not_in.
  left. split. trivial. auto with local.
  destruct or; try discriminate; not_in_absurd H.
  destruct or; try discriminate; not_in_absurd H.
  auto with local. constructor.

  (* step 4 : [C] <G- [W; G] => [C; G] || [W] *)
  apply goLeft with (l := [Cabbage]) (r := [Wolf; Goat]).
  exists Goat.
  try repeat (split; jauto).
  destruct or; intros H; not_in_absurd H.
  right. split. discriminate. split. auto with local. auto_not_in.
  left. split. trivial. auto with local.
  destruct ol; try discriminate; not_in_absurd H.
  auto with local.

  (* step 3 : [W; C] -W> [G] => [C] || [W; G] *)
  apply goRight with (l := [Wolf; Cabbage]) (r := [Goat]).
  exists Wolf.
  try repeat (split; jauto).
  destruct ol; intros H; not_in_absurd H.
  left. split. trivial. auto with local.
  right. split. discriminate. split. auto with local. auto_not_in.
  destruct or; try discriminate; not_in_absurd H.
  auto with local.

  (* step 2 : [W; C] <- [G] => [W; C] || [G] *)
  apply goLeftAlone.
  auto with local.

  (* step 1 : [W; G; C] -G> [] => [W; C] || [G] *)
  apply goRight with (l := [Wolf; Goat; Cabbage]) (r := []).
  exists Goat.
  try repeat (split; jauto).
  destruct ol; intros H; not_in_absurd H.
  right. split. discriminate. split. auto with local. auto_not_in.
  left. split. trivial. auto with local.
  right. split. discriminate. split. auto with local. auto_not_in.
  auto with local.

  (* initial state: everyone on the left bank *)
  apply initial; repeat auto with local.
Qed.
```

```alectryon
End SecondAttempt.
```

# Conclusions and Issues

In the end, I was surprised that I was able to produce something that
at least resembled a model and proof to the problem. I thought I'd
need much more to even begin dealing with this puzzle, especially
after failing to find an example solution.

But there are still some issues that I cannot address on my own, given
my current knowledge.

1. I'm not sure if there is another "hole" in my logic, as the first
   time everything fit into place but was wrong.
2. The `RiverCrossing` predicate and the solution still seem overly
   complicated. I feel that they should be shorter and cleaner...
3. Both proofs are backwards. I'd like to somehow start from the
   initial state and build my proof towards the desired goal. I guess
   one way is to "reverse" the constructors in `RiverCrossing` and
   start from the initial state in the solution goal. But that seems
   wrong.
4. I still need to specify the initial left and right river banks on
   the steps that carry an object. I expected that I could use
   `eapply` or `eauto` and let Coq infer those later, yet that didn't
   work out. This is kinda annoying.
5. If I were to use
   [bullets](https://coq.inria.fr/refman/proofs/writing-proofs/proof-mode.html#bullets)
   when building the proof steps, each new step would get nested
   deeper and deeper, making it harder to read. So I just separated
   them the way I did. I'd love to know how to structure proofs of
   this form in a more elegant way...

I [shared this problem in the Coq
Zulip](https://coq.zulipchat.com/#narrow/stream/237977-Coq-users/topic/.5Bnewbie.5D.20wolf.2C.20goat.20and.20cabbage.20puzzle/near/252471119)
chat asking for advice, but unfortunately, at the time of writing, no
one has replied yet. Maybe someone will shed light on those issues
someday, and I hope to learn enough to address them in the (distant)
future...
