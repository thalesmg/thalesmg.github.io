#lang pollen

◊(define-meta title "Stateful Parallel Property Testing Example in Elixir")
◊(define-meta date "2022-08-15")
◊(define-meta tags ("elixir" "stateful property based testing" "concurrency"))


◊p{I think the first time I heard about stateful property testing was in
this excellent blog post◊numbered-note{◊hyperlink["https://medium.com/@tylerneely/reliable-systems-series-model-based-property-testing-e89a433b360"]{Reliable Systems Series: Model-Based Testing - Tyler Neely}}. I found the idea very
interesting, but only managed to apply the idea (very poorly) once.
Recently, I saw two great talks about stateful property based
testing◊numbered-note{◊hyperlink["https://www.youtube.com/watch?v=zi0rHwfiX1Q"]{Testing the Hard Stuff and Staying Sane - John Hughes}} ◊numbered-note{◊hyperlink["https://www.youtube.com/watch?v=q0wZzFUYCuM"]{Introduction to Stateful Property Testing - Tomasz Kowal}}, and was fascinated by QuickCheck's
ability to test parallelism errors.}

◊p{I tried to search for some toy examples of such stateful parallel
testing, but had some difficulty finding one◊numbered-note{It seems that people use parallel execution so
seldomly that I even found a ◊hyperlink["https://github.com/alfert/propcheck/pull/214"]{couple} ◊hyperlink["https://github.com/alfert/propcheck/pull/214#issuecomment-1214483621"]{bugs} when exploring this simple
example.}, as
most examples use sequential execution. Also, I find the callback
descriptions and typespecs from PropEr a bit confusing as to what I
should do on each one. The introduction of the module does describe
them, but I got confused when trying to look at the typespecs. So,
for my own reference, I tried to simply reproduce Hughes' ticket
dispenser example in Elixir, using ◊hyperlink["https://github.com/alfert/propcheck/"]{PropCheck}, which is an Elixir
wrapper around ◊hyperlink["https://github.com/proper-testing/proper"]{PropEr}.}

◊p{The ticket dispenser has two operations: ◊code{take_ticket} and ◊code{reset}.
The first version needs to have a concurrency bug, obviously:}

◊highlight['elixir]{
defmodule TicketDispenserTest do
  use ExUnit.Case, async: true
  use PropCheck, default_opt: &PropCheck.TestHelpers.config/0
  use PropCheck.StateM

  @issue_ticket {:call, __MODULE__, :take_ticket, []}
  @reset {:call, __MODULE__, :reset, []}

  property "issues unique tickets", [:verbose, numtests: 1_000] do
    forall cmds <- parallel_commands(__MODULE__) do
      {:ok, pid} = Agent.start_link(fn -> 0 end)
      Process.register(pid, Counter)
      {seq_history, par_history, result} = run_parallel_commands(__MODULE__, cmds)
      Agent.stop(pid)

      (result == :ok)
      |> aggregate(command_names(cmds))
      |> when_fail(
        (
          PropCheck.StateM.Reporter.print_report({seq_history, par_history, result}, cmds)
          IO.puts("Result: #{inspect(result)}")
        )
      )
    end
  end

  def take_ticket() do
    x = Agent.get(Counter, & &1)
    Agent.update(Counter, fn _ -> x + 1 end)
    x
  end

  def reset() do
    Agent.update(Counter, fn _ -> 0 end)
    :ok
  end

  def initial_state(), do: 0
  def command(_state), do: oneof([@issue_ticket, @reset])
  def next_state(state, _res, @issue_ticket), do: state + 1
  def next_state(_state, _res, @reset), do: 0
  def postcondition(state, @issue_ticket, res), do: state == res
  def postcondition(_state, @reset, _res), do: true
  def precondition(_state, @issue_ticket), do: true
  def precondition(_state, @reset), do: true
end
}

◊p{The ◊code{command/1} callback must return a ◊emph{generator} for the commands
you want to define, which in turn is a ◊emph{symbolic call} of the form
◊code{{:call, module :: atom(), fun :: atom(), [term()]}} (not what the
◊code{command()} typespec suggests it would be). Another detail to have in
mind (which is described the ◊code{proper_statem} introduction) is that the
◊code{postcondition/3} callback takes in the state ◊emph{prior to command
execution}.}

◊p{Instead of using ◊code{Agent.get_and_update/1} to atomically update the
state while issuing a new ticket, we do it in separate steps. That
way, we have an unprotected critical section to see how
PropCheck/PropEr behaves.}

◊p{Indeed, it does manage to quickly find and shrink to a very minimal
failing example:}

◊highlight{
================================================================================
Concurrency Failure, we don't show the state :-/

Sequential commands:
   var1 = TicketDispenserTest.reset()
        # -> :ok
        # Post state: 0


Process 1:
   var2 = TicketDispenserTest.reset()
        # -> :ok
   var3 = TicketDispenserTest.take_ticket()
        # -> 0
   var4 = TicketDispenserTest.reset()
        # -> :ok
   var5 = TicketDispenserTest.reset()
        # -> :ok
   var6 = TicketDispenserTest.reset()
        # -> :ok


Process 2:
   var7 = TicketDispenserTest.take_ticket()
        # -> 0
   var8 = TicketDispenserTest.take_ticket()
        # -> 1
   var9 = TicketDispenserTest.take_ticket()
        # -> 0
   var10 = TicketDispenserTest.take_ticket()
        # -> 1
   var11 = TicketDispenserTest.take_ticket()
        # -> 2
   var12 = TicketDispenserTest.take_ticket()
        # -> 3



Result: :no_possible_interleaving

Shrinking .......(7 time(s))
{[],
 [
   [{:set, {:var, 3}, {:call, TicketDispenserTest, :take_ticket, []}}],
   [{:set, {:var, 8}, {:call, TicketDispenserTest, :take_ticket, []}}]
 ]}

================================================================================
Concurrency Failure, we don't show the state :-/

Sequential commands:


Process 1:
   var3 = TicketDispenserTest.take_ticket()
        # -> 0


Process 2:
   var8 = TicketDispenserTest.take_ticket()
        # -> 0
}

◊p{So, the shrunk counter-example shows that we have no sequential prefix
(so, no shared sequence of commands before the two processes run
concurrently), and they simply each try to take a ticket concurrently.
Then it fails with the ◊code{:no_possible_interleaving} error since,
according to the model, there's no way that both processes manage to
get the same ticket.}

◊p{As mentioned before, after the simple fix...}

◊highlight['elixir]{
def take_ticket() do
-  x = Agent.get(Counter, & &1)
-  Agent.update(Counter, fn _ -> x + 1 end)
-  x
+  Agent.get_and_update(Counter, & {&1, &1 + 1})
end
}

◊p{... the test passes:}

◊highlight{
OK: Passed 1000 test(s).

50.06% {TicketDispenserTest, :reset, 0}
49.94% {TicketDispenserTest, :take_ticket, 0}
.

Finished in 1.9 seconds (1.9s async, 0.00s sync)
1 property, 0 failures
}

◊p{One more toy example: the classic bank transaction. Here, I'll model
it with just one ◊code{transfer} command, and the initial state will
contain some cash on three predetermined accounts. I'll put the real
implementation in a separate ◊code{Transaction} module (the reason for this
will be clear shortly). For the initial obviously buggy version,
we'll use Mnesia dirty reads/writes without any transactions:}

◊highlight['elixir]{
defmodule Transaction do
  def transfer(from, to, amount) do
    [{_, _, prev_from}] = :mnesia.dirty_read({:balances, from})
    [{_, _, prev_to}] = :mnesia.dirty_read({:balances, to})
    if prev_from >= amount do
      :mnesia.dirty_write({:balances, from, prev_from - amount})
      :mnesia.dirty_write({:balances, to, prev_to + amount})
      :ok
    else
      :no_funds
    end
  end
end
}

◊highlight['elixir]{
defmodule BankTransactionTest do
  use ExUnit.Case, async: true
  use PropCheck, default_opt: &PropCheck.TestHelpers.config/0
  use PropCheck.StateM

  setup_all do
    :ok = :mnesia.start()
    :ok
  end

  property "issues unique tickets", [:verbose, numtests: 1_000] do
    {:atomic, :ok} = :mnesia.create_table(:balances, [])

    forall cmds <- parallel_commands(__MODULE__) do
      Enum.each([:a, :b, :c], fn account ->
        :mnesia.dirty_write({:balances, account, initial_balance()})
      end)

      {seq_history, par_history, result} = run_parallel_commands(__MODULE__, cmds)
      tab = :ets.tab2list(:balances)
      :mnesia.clear_table(:balances)

      (result == :ok)
      |> aggregate(command_names(cmds))
      |> when_fail(
        (
          PropCheck.StateM.Reporter.print_report({seq_history, par_history, result}, cmds)
          IO.puts("Result: #{inspect(result)}")
          IO.puts("Accounts: #{inspect(tab, pretty: true)}")
        )
      )
    end
  end

  def initial_state(),
    do: %{
      a: initial_balance(),
      b: initial_balance(),
      c: initial_balance()
    }

  def command(_state),
    do:
      oneof([
        {:call, Transaction, :transfer, [account(), account(), integer(1, 10)]}
      ])

  def next_state(state, res, {:call, Transaction, :transfer, [from, to, amount]}) do
    if Map.fetch!(state, from) >= amount do
      state
      |> Map.update!(from, &(&1 - amount))
      |> Map.update!(to, &(&1 + amount))
    else
      state
    end
  end

  def postcondition(state, {:call, Transaction, :transfer, [from, to, amount]}, res) do
    case res do
      :ok -> Map.fetch!(state, from) >= amount
      :no_funds -> Map.fetch!(state, from) < amount
    end
  end

  def precondition(state, {:call, Transaction, :transfer, [from, to, amount]}) do
    from != to
  end

  defp initial_balance(), do: 3

  defp account(), do: oneof([:a, :b, :c])
end
}



◊p{Here, we set a pre-condition on the transfer so we don't transfer from
an account to itself. That'll guide the command generation phase.}

◊p{Let's see it break!}

◊highlight{
OK: Passed 1000 test(s).

100.0% {Transaction, :transfer, 3}
.

Finished in 2.7 seconds (2.7s async, 0.00s sync)
1 property, 0 failures
}

◊p{What?! That implementation is obviously broken, yet all tests
passed? How come?}

◊p{The issue is that this concurrency bug depends on special
interleavings of the processes, which are rare in the schedulings of
most executions, in particular if the system is not under heavy
stress.}

◊p{To increase the chance of such rare interleavings, we have to sprinkle
◊code{:erlang.yield/0} calls between function calls which might lead to
race conditions. Luckily, PropCheck has some facilities to do that
for us: we just have to instrument the implementation module with
◊code{Instrument.instrument_module(Impl, YieldInstrumenter)}.
Unfortunately, Mnesia functions are not among the default functions
targeted by ◊code{Instrumenter}. We simply need to make our own
instrumenter extending the default one:}



◊highlight['elixir]{
defmodule PropCheck.MyYieldInstrumenter do
  require Logger

  alias PropCheck.Instrument

  @behaviour Instrument

  @impl true
  def handle_function_call(call) do
    Logger.debug("handle_function: #{inspect(call)}")
    Instrument.prepend_call(call, Instrument.call_yield())
  end

  @impl true
  def is_instrumentable_function(mod = {:atom, _meta1, module}, fun = {:atom, _, function}) do
    [
      {:mnesia, :dirty_read},
      {:mnesia, :dirty_write},
      {:mnesia, :read},
      {:mnesia, :wread},
      {:mnesia, :write},
      {:mnesia, :transaction}
    ]
    |> MapSet.new()
    |> MapSet.member?({module, function})
    |> Kernel.||(Instrument.instrumentable_function(mod, fun))
  end
  def is_instrumentable_function(_, _), do: false
end
}



◊p{And then we instrument our module before running the tests. That's
why I put the code in a separate module that's compiled (not an ◊code{.exs}
file):}

◊highlight['elixir]{
setup_all do
+ Instrument.instrument_module(Transaction, MyYieldInstrumenter)
  :ok = :mnesia.start()
  :ok
end
}

◊p{Alternatively, we could sprinkle the ◊code{:erlang.yield()} calls
ourselves.}

◊p{Running the instrumented code then yields the expected failure:}

◊highlight{
!
Failed: After 1 test(s).

# ✀ -- snip -- ✀

Shrinking ...(3 time(s))
{[],
 [
   [{:set, {:var, 2}, {:call, Transaction, :transfer, [:a, :c, 3]}}],
   [{:set, {:var, 4}, {:call, Transaction, :transfer, [:a, :b, 3]}}]
 ]}

================================================================================
Concurrency Failure, we don't show the state :-/

Sequential commands:


Process 1:
   var2 = Transaction.transfer(:a, :c, 3)
        # -> :ok


Process 2:
   var4 = Transaction.transfer(:a, :b, 3)
        # -> :ok



Result: :no_possible_interleaving
Accounts: [{:balances, :c, 6}, {:balances, :b, 6}, {:balances, :a, 0}]
}

◊p{The minimal failing test in this case is simple: two processes try to
transfer from the same account concurrently. Wrapping everything in a
transaction should fix the problem:}



◊highlight['elixir]{
defmodule Transaction do
  def transfer(from, to, amount) do
    {:atomic, res} = :mnesia.transaction(fn ->
      [{_, _, prev_from}] = :mnesia.wread({:balances, from})
      [{_, _, prev_to}] = :mnesia.wread({:balances, to})
      if prev_from >= amount do
        :mnesia.write({:balances, from, prev_from - amount})
        :mnesia.write({:balances, to, prev_to + amount})
        :ok
      else
        :no_funds
      end
    end)
    res
  end
end
}



◊highlight{
OK: Passed 1000 test(s).

100.0% {Transaction, :transfer, 3}
.

Finished in 11.5 seconds (11.5s async, 0.00s sync)
1 property, 0 failures
}

◊p{Very cool! I hope to be able to apply more of this to
my real world problems! 🍻}
