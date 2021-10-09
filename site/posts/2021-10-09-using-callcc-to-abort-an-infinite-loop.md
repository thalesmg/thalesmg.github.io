---
title: Using callCC to abort an infinite loop
tags: haskell, continuations, callcc,
---

This is just a quick complement to Drew Olson's article ["Adventures
in Looping"](https://blog.drewolson.org/adventures-in-looping).
There, they describe some approaches to exiting an otherwise infinite
loop in Haskell, and gives a nice overview of using some constructs
like `MaybeT`, `mzero`, `void` and `forever`.  I recommend reading it!
It was also discussed on this week's [Haskell Weekly
podcast](https://haskellweekly.news/episode/54.html).

The problem described there is that of an infinite loop of reading
some data from somewhere and then doing some processing of that, but
sometimes exiting that loop to, say, reconnect and start over.  The
final solution presented there is quite clean and uses the `MonadPlus`
instance for `MaybeT` to abort the loop without introducing explicit
recursion.  The folks at Haskell Weekly podcast also suggest using
something like
[`ExceptT`](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Except.html#t:ExceptT)
to allow returning some information about why to loop was broken.

Thinking about this, I realized that it seemed like an apt use case
for
[`callCC`](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Cont.html#v:callCC)!
As described in its documentation, `callCC` provides an escape
continuation that allows aborting the current computation early and
returning some result.  It behaves quite similar to `return`
statements from imperative languages, but inside the `ContT` monad.
Since this approach was not mentioned in neither the blog post nor the
podcast episode, I wanted to share this slight alternative.  Also,
because it seems to me that it is hard to find some practical example
usages of `ContT` / `callCC`, and this looks like a perfect fit for
it.

The resulting code is almost identical to the `MaybeT` version:

```haskell
-- runContT :: ContT r m a -> (a -> m r) -> m r
-- callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a

main :: IO ()
main = forever $ do
  wsUrl <- fetchConnectionUrl
  conn <- connectWebSocket wsUrl

  void . flip runContT pure $ callCC $ \abort -> forever $ do
    message <- liftIO $ readMessage conn

    case message of
      MessageA   -> liftIO $ putStrLn "Message A"
      MessageB   -> liftIO $ putStrLn "Message B"
      Disconnect -> do
        liftIO $ putStrLn "Disconnect!"
        abort ()
```

The differences are the use of `runContT` and `callCC`, since we are
using the continuation monad.  The `pure` argument to `flip runContT`
is the final continuation that will receive the result from running
the `ContT r m a`, which in turn corresponds to `r` in that type.
`callCC` must be fed a function, which itself receives another
function as an argument (this was a bit confusing for me in the
beginning!).  This argument is the so-called escape continuation that,
if invoked with some value, will abort any remaining computation and
just return the value fed to it.

Running it does indeed work as intended (just printing some messages
to emulate the program):

```
> main
connecting...
Message B
Disconnect!
connecting...
Message B
Disconnect!
connecting...
Message B
Message A
Message A
Disconnect!
connecting...
Disconnect!
^CInterrupted.
```

If we wanted to return some information about the reason we are
aborting the loop, as would be case if we used `ExceptT`, we can also
do that by providing the reason to `abort`:

```haskell
main2 :: IO ()
main2 = forever $ do
  wsUrl <- fetchConnectionUrl
  conn <- connectWebSocket wsUrl

  AbortReason reason <- flip runContT pure $ callCC $ \abort -> forever $ do
    message <- liftIO $ readMessage conn

    case message of
      MessageA   -> liftIO $ putStrLn "Message A"
      MessageB   -> liftIO $ putStrLn "Message B"
      Disconnect -> do
        liftIO $ putStrLn "Disconnect!"
        abort $ AbortReason "something went wrong!"
  putStrLn $ "disconnected! reason: " <> reason
```

And then:

```
> main2
connecting...
Disconnect!
disconnected! reason: something went wrong!
connecting...
Message B
Disconnect!
disconnected! reason: something went wrong!
connecting...
Message A
Message B
Message B
Disconnect!
disconnected! reason: something went wrong!
connecting...
^CInterrupted.
```

That is it!  I'd like to see more practical examples of `ContT` and
`callCC` whenever possible.  =)

These alternative versions are available in [this
gist](https://gist.github.com/thalesmg/b63781002db9f2289db3fc393bb4c3f4).
