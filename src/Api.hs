{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Api
( API
) where

import Data.Text
import Servant
import People.Person (Person)

type API = "people" :> Get '[JSON] [Person]
      :<|> "person" :> Capture "id" Int :> Get '[JSON] Person
