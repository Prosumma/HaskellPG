# Piggy

Piggy is a tiny little Haskell library that makes Database.PostgreSQL.Simple just a tiny bit easier to work with.

It uses the [Operational monad](https://hackage.haskell.org/package/operational-0.2.3.5/docs/Control-Monad-Operational.html), a simple typeclass, and a convenience newtype. The Operational monad and typeclass allow replacing database operations with stubs that do not actually talk to PostgreSQL.

```haskell
class Monad m => MonadPG m where
  type ConnectionType m
  getConnection :: m (ConnectionType m)
  interpretPG :: PGDSL a -> m a
```
