{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module TaskWarriorWeb.Utility where
import qualified Data.Aeson.Types as T
import Data.Either (rights)

class f :~> g where
  ntrans :: f a -> g a
  
instance T.Result :~> Maybe where
  ntrans (T.Error _) = Nothing
  ntrans (T.Success x) = Just x

instance T.Result :~> Either String where
  ntrans (T.Error x) = Left x
  ntrans (T.Success x) = Right x
  
instance (Either e) :~> [] where
  ntrans = rights . (:[])