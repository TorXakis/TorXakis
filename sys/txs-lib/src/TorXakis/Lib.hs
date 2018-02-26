-- |

module TorXakis.Lib where

import           Data.Text (Text)

data Response = Success | Error { msg :: Text }

data FileName

data FileContents

data ModelName

-- | The session monad, which maintains the state of a TorXakis model.
data Session a

-- | Load a TorXakis file, compile it, and return the response.
load :: FileName -> FileContents -> Session Response
load = undefined

-- | Start the stepper with the current model.
stepper :: ModelName -> Session Response
stepper = undefined

-- | How a step is described
data StepType = Action ActionName
              | Run Steps
              | GoTo StateNumber
              | Reset -- ^ Go to the initial state.
              | Rewind Steps

-- TODO: discuss with Jan: do we need a `Tree` step here?

data StateNumber
data ActionName
-- | Number of steps
data Steps

-- | step for n-steps
step :: StepType -> Session Response -- | Or a conduit?
step = undefined
