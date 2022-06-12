# Piggy

Piggy is a tiny little Haskell library that makes Database.PostgreSQL.Simple just a tiny bit easier to work with. It also facilitates easier unit testing. 

It uses the [Operational monad](https://hackage.haskell.org/package/operational-0.2.3.5/docs/Control-Monad-Operational.html) and a simple typeclass:

```haskell
class Monad m => MonadPG m where
  interpret :: PGDSL a -> m a
  withTransaction :: m a -> m a
  withTransaction t = t
```

A default interpreter &mdash; `interpg` &mdash; that talks to a Postgres database is provided, as is `PG`, a default implementation of `MonadPG` that actually talks to a database using `interpg`. 

In unit tests, it's relatively straightforward to replace actual database access with stubs.

```haskell
getPatients :: MonadPG m => m [Patient]
getPatients = interpret $ singleton (Query_ "SELECT * FROM patients")

patients :: [Patient]
-- etc.

interstubs :: Monad m => PGDSL a -> m a
interstubs m = case view m of
  (Query_ "SELECT * FROM patients") :>>= k -> return patients >>= interstubs . k
  -- etc.

newtype PGStub a = PGStub a deriving (Functor, Applicative, Monad)

-- If `getPatients` is called with `PGStub`, it will be interpreted with
-- interstubs and no actual database access will occur. In fact, the
-- function is actually pure because it does not require `MonadIO`.
instance MonadPG PGStub where
  interpret = interstubs
```

