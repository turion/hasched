module LP.Imports
    ( module X
    , LPSeminarFun
    ) where

-- glpk-hs
import Control.Monad.LPMonad          as X
import Data.LinearProgram             as X
import Data.LinearProgram.GLPK.Solver as X

-- base
import Control.Monad as X

-- hasched
import Stundenplan as X
import LP.Utils    as X

-- TODO Ist Double auch für die ganzzahligen Werte ok?
type LPSeminarFun = Seminar -> LPM String Double ()
