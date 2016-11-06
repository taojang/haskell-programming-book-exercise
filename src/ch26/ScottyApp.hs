{-# LANGUAGE OverloadedStrings #-}
module ScottyApp where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config
  {
    -- that's one, one click!
    -- two...two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }


-- Stuff inside ScottyT is, except for things that escape
-- via IO, effectively read-only so we can't use StateT.
-- It would overcomplicate things to attempt to do so and
-- you should be using a proper database for production
-- applications.
type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let m' = M.unionWith (+) m $ M.fromList [(k, 1)]
  in fromMaybe 1 <$> (m', M.lookup k m')


app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    conf       <- lift ask
    let key' = mappend (prefix conf) unprefixed
    newInteger <- liftIO $ atomicModifyIORef' (counts conf) $ bumpBoomp key'
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR = flip runReaderT config
  scottyT 3000 runR app
