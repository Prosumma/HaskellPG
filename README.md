# Piggy

Piggy is a tiny little Haskell library that makes Database.PostgreSQL.Simple just a tiny bit easier to work with. It also facilitates easier unit testing. 

It uses the [Operational monad](https://hackage.haskell.org/package/operational-0.2.3.5/docs/Control-Monad-Operational.html), two simple typeclasses, and a convenience newtype. The Operational monad and `MonadPG` typeclass allow replacing database operations with stubs that do not actually talk to PostgreSQL.

```haskell
class Monad m => MonadPG m where
  interpret :: PGDSL a -> m a
```

The other typeclass is `MonadConnection`:

```haskell
class Monad m => MonadConnection m where
  getConnection :: m Connection
```

This is only needed for types which actually talk to the database, such as `PG`. This typeclass should be regarded as an implementation detail. Functions which perform database operations do not need to mention it, e.g.,

```haskell
getPatientById :: (MonadPG m, MonadThrow m) => Int -> m Patient
getPatientById id = query1 "SELECT * FROM patient WHERE id = ?" id
```

The DSL is very simple, consisting of only 4 operations.

```haskell
data Operation a where
  Execute :: ToRow q => Query -> q -> Operation Int64
  Execute_ :: Query -> Operation Int64 
  Query :: (ToRow q, FromRow r) => Query -> q -> Operation [r]
  Query_ :: FromRow r => Query -> Operation [r]
```

This makes it very easy to write an alternate interpreter for unit tests.