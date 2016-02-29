{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Api
( API
) where

import Servant
import People.Person (Person)

type API = "people" :> Get '[JSON] [Person]
