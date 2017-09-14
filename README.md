# hasql-simple

A somewhat opinionated "simpler" API to hasql. This allows to write code like:

```haskell
import Hasql.Query
import Hasql.Simple
import qualified Hasql.Decoders as D

data NewUser
    = NewUser
    { nu_username :: !T.Text
    , nu_email :: !T.Text
    , nu_password :: !(Password 'PtHash)
    } deriving (Show)

createUserQ :: Query NewUser UserId
createUserQ =
    statement sql encoder decoder True
    where
      sql =
          "INSERT INTO login (username, email, password) VALUES ($1, $2, $3) RETURNING id;"
      encoder =
          req nu_username
          <> req nu_email
          <> req nu_password
      decoder =
          D.singleRow $ D.value dbDec
```

The `-simple` in the name is due to this type class approach beeing similar to the one found in `postgresql-simple`. All contributions (e.g. more instances and/or helper functions) are welcome, please send a PR.
