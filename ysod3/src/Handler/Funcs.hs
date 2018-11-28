module Handler.Funcs where

import Yesod
import Foundation
import Data.Text as T
import Text.Show as S

anyOriginIn ::  Handler ()
anyOriginIn  = do
    addHeader (T.pack "Access-Control-Allow-Origin") (T.pack "*")
    addHeader (T.pack "Access-Control-Allow-Methods")  (T.pack  "OPTIONS,PATCH,GET,POST")
    addHeader (T.pack "Access-Control-Allow-Headers") (T.pack "Origin, X-Requested-With, Content-Type, Accept, Authorization")
    addHeader  (T.pack "Access-Control-Expose-Headers") (T.pack "Authorization")
    
