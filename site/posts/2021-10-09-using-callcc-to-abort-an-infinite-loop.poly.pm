#lang pollen

◊(define-meta title "Using callCC to abort an infinite loop")
◊(define-meta date "2021-10-09")
◊(define-meta tags ("haskell" "continuations" "callcc"))

◊p{This is just a quick complement to Drew Olson’s article ◊hyperlink["https://blog.drewolson.org/adventures-in-looping"]{}.
There, they describe some approaches to exiting an otherwise infinite
loop in Haskell, and gives a nice overview of using some constructs
like ◊code{MaybeT}, ◊code{mzero}, ◊code{void} and ◊code{forever}. I recommend reading it!
It was also discussed on this week’s ◊hyperlink["https://haskellweekly.news/episode/54.html"]{Haskell Weekly
podcast}.}

◊p{The problem described there is that of an infinite loop of reading
some data from somewhere and then doing some processing of that, but
sometimes exiting that loop to, say, reconnect and start over. The
final solution presented there is quite clean and uses the ◊code{MonadPlus}
instance for ◊code{MaybeT} to abort the loop without introducing explicit
recursion. The folks at Haskell Weekly podcast also suggest using
something like
◊hyperlink["https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Except.html#t:ExceptT"]{◊code{ExceptT}}
to allow returning some information about why to loop was broken.}

◊p{Thinking about this, I realized that it seemed like an apt use case
for
◊hyperlink["https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Cont.html#v:callCC"]{◊code{callCC}}!
As described in its documentation, ◊code{callCC} provides an escape
continuation that allows aborting the current computation early and
returning some result. It behaves quite similar to ◊code{return}
statements from imperative languages, but inside the ◊code{ContT} monad.
Since this approach was not mentioned in neither the blog post nor the
podcast episode, I wanted to share this slight alternative. Also,
because it seems to me that it is hard to find some practical example
usages of ◊code{ContT} / ◊code{callCC}, and this looks like a perfect fit for
it.}

◊p{The resulting code is almost identical to the ◊code{MaybeT} version:}

◊highlight['haskell]{
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
}

◊p{The differences are the use of ◊code{runContT} and ◊code{callCC}, since we are
using the continuation monad. The ◊code{pure} argument to ◊code{flip runContT}
is the final continuation that will receive the result from running
the ◊code{ContT r m a}, which in turn corresponds to ◊code{r} in that type.
◊code{callCC} must be fed a function, which itself receives another
function as an argument (this was a bit confusing for me in the
beginning!). This argument is the so-called escape continuation that,
if invoked with some value, will abort any remaining computation and
just return the value fed to it.}

◊p{Running it does indeed work as intended (just printing some messages
to emulate the program):}

◊highlight['haskell]{
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
}

◊p{If we wanted to return some information about the reason we are
aborting the loop, as would be case if we used ◊code{ExceptT}, we can also
do that by providing the reason to ◊code{abort}:}

◊highlight['haskell]{
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
}

◊p{And then:}

◊highlight['haskell]{
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
}

◊p{That is it! I’d like to see more practical examples of ◊code{ContT} and
◊code{callCC} whenever possible. =)}

◊p{These alternative versions are available in ◊hyperlink["https://gist.github.com/thalesmg/b63781002db9f2289db3fc393bb4c3f4"]{this
gist}.}
