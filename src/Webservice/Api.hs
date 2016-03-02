{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Webservice.Api
( API
) where

import Servant
import Person.Person (Person)

type API = "people" :> Get '[JSON] [Person]
      :<|> "person" :> Capture "id" Int :> Get '[JSON] Person
