---
title: Carpe Diem
---

Fusce tortor quam, egestas in posuere quis, porttitor vel turpis. Donec
vulputate porttitor augue at rhoncus. Proin iaculis consectetur sagittis.
Curabitur venenatis turpis sit amet purus tristique nec posuere risus laoreet.
Nullam nisi sem, dapibus id semper id, egestas vel arcu. Morbi porttitor ipsum
placerat erat consequat sed consequat purus feugiat. Donec auctor elit ut risus
mattis facilisis. Lorem ipsum dolor sit amet, consectetur adipiscing elit.

Proin vulputate sapien facilisis leo ornare pulvinar. Fusce tempus massa a risus
semper iaculis. Suspendisse sollicitudin posuere nunc, sit amet rutrum leo
facilisis mattis. Sed ornare auctor dui, vitae rutrum neque auctor sit amet.
Proin ac dui magna. Mauris vehicula interdum augue, nec ultrices libero egestas
quis. Nunc convallis euismod ipsum, id sollicitudin orci consequat ac. Fusce
bibendum congue libero, in rutrum nulla congue non. Cras sit amet risus tortor,
eu pellentesque dui. Phasellus euismod enim non nibh sodales quis consectetur
lorem laoreet. Vivamus a egestas quam. Curabitur in tortor augue, vitae varius
tellus. Integer varius, elit ac gravida suscipit, eros erat pellentesque nisi,
et tristique augue odio id nulla. Aliquam sit amet nunc vel tellus hendrerit
tempus ac vel sem.

Aenean tincidunt sollicitudin sapien ut porttitor. Curabitur molestie adipiscing
lorem vel scelerisque. Donec vitae interdum est. Proin rutrum vulputate
faucibus. Suspendisse sit amet felis odio, non volutpat ante. Sed eu lectus
quam. Curabitur tristique rhoncus est, vel commodo tortor suscipit semper.
Maecenas feugiat vestibulum nisi id facilisis. Nulla non tincidunt libero.
Praesent ultrices interdum commodo. Sed euismod nisl auctor leo ultrices rutrum.
Aliquam nibh felis, congue molestie blandit at, bibendum at eros. Aenean
tincidunt, tortor iaculis placerat sollicitudin, lorem justo tempor diam, et
posuere sapien leo et magna. Quisque vel aliquam mauris.

Proin varius tempus fermentum. Cum sociis natoque penatibus et magnis dis
parturient montes, nascetur ridiculus mus. Sed tincidunt nunc id magna
adipiscing non sollicitudin turpis tempor. Etiam vel elit ipsum, quis euismod
velit. Quisque elementum magna vitae quam venenatis lacinia. Sed at arcu ipsum.
Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos
himenaeos. Donec ut lorem ac sapien cursus lacinia sit amet mollis dolor.
Vivamus tempus odio nec magna faucibus sed hendrerit lorem tempor.

Vestibulum eu nisi arcu. Curabitur nisi risus, fermentum ut lacinia ut, interdum
nec magna. Nunc aliquet gravida massa, eu aliquam lorem faucibus at. Sed
sollicitudin volutpat velit id tempor. In nibh justo, pharetra et pretium
dignissim, tempus in turpis. Phasellus eget lobortis nisl. Phasellus sed
fermentum diam. Nam tempus pharetra odio, quis congue eros imperdiet eu. Aliquam
dui eros, hendrerit et vulputate vel, porta eu eros. Nullam nisi dui, commodo
eget pharetra ut, ornare sit amet nunc. Fusce vel neque urna. Maecenas nulla
ante, egestas at consequat quis, fermentum a enim. Aliquam id tristique urna.
Integer augue justo, scelerisque et consectetur id, rhoncus eget enim.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (blah) where

import qualified Data.Map as M

-- comentário

-- | haddock
data Bah m = MkBah { bah :: m Text
                   , boh :: Int -> IO ()
                   }

newtype Blergh = MkBlergh { getBlerg :: () }

cousa :: Cont r Int -> Cont r String
cousa c | c > 10 = fmap show c
        | otherwise = undefined
```

```clojure
;; comentário
(def coisa
  (1 2 3))

(defn uma-coisa-marota
  [a b]
  (->>
   :a
   {:b 123 :c 1M}
   [1 2 3]
   (assoc (when ) (if true 1 2)))
  {:b a
   :a b})
```

```elixir
  # comentário
  plt = :dialyzer_plt.from_file "_build/dev/dialyxir_erlang-22.3.4.10_elixir-1.10.4_deps-dev.plt"

  all_modules = plt |> :dialyzer_plt.all_modules() |> :sets.to_list()

  :dialyzer_plt.lookup_module(plt, Ren.Util.Range.Logic)

  files = for app <- [:elixir, :ex_unit, :mix, :iex, :logger, :eex] ++ [:erts, :kernel, :stdlib, :compiler],
          path <- Path.join([Application.app_dir(app), "**/*.beam"]) |> Path.wildcard() do
      to_charlist(path)
    end

  output_plt = "priv/plts/hallux.plt"
  :dialyzer.run(
    analysis_type: :plt_build,
    files: files,
    from: :byte_code,
    output_plt: to_charlist(output_plt)
  )

  my_plt = :dialyzer_plt.from_file(output_plt)
  all_modules = my_plt |> :dialyzer_plt.all_modules() |> :sets.to_list()


  Application.ensure_loaded(:dialyxir)
  output_plt = "priv/plts/hallux.plt"
  :dialyzer.run(
    analysis_type: :plt_build,
    files: Dialyxir.Project.dialyzer_files(),
    from: :byte_code,
    output_plt: to_charlist(output_plt)
  )
  my_plt = :dialyzer_plt.from_file(output_plt)
  all_modules = my_plt |> :dialyzer_plt.all_modules() |> :sets.to_list()
  {:value, coisas} = :dialyzer_plt.lookup_module(my_plt, Hallux.Internal.Node)

  analysis = with {:value, coisas} <- :dialyzer_plt.lookup_module(my_plt, Hallux.Internal.Node) do
               for {{mod, fun, ari}, ret, args} <- coisas, into: %{} do
                 t = :erl_types.t_fun(args, ret)
                 sig = :dialyzer_utils.format_sig(t)
                 IO.inspect(
                   ret: ret,
                   args: args,
                   t: t
                 )
                 {{mod, fun, ari}, to_string(sig)}
               end
             end

defmodule Bah do
  def bah(x, y) do
    {:ok, x + y}
  end
end
```

<script src="https://gist.github.com/thalesmg/3fdc4ed2ad8f406949b475362a6a2e1a.js"></script>

<script src="https://gist.github.com/thalesmg/24dfdae87f0e5ddbda34d5cec8cc63a6.js"></script>
