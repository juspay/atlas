{- |
Copyright 2022 Juspay Technologies Pvt Ltd

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Module      :  Fixtures.Person
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Fixtures.Person where

import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultDriver :: Person.Person
defaultDriver =
  Person.Person
    { id = Id "1",
      firstName = "Driver",
      middleName = Nothing,
      lastName = Just "Driverson",
      role = Person.DRIVER,
      gender = Person.UNKNOWN,
      identifierType = Person.EMAIL,
      email = Just "driverson@cool-drivers.com",
      mobileNumber = Nothing,
      mobileCountryCode = Nothing,
      passwordHash = Nothing,
      identifier = Nothing,
      rating = Nothing,
      isNew = True,
      udf1 = Nothing,
      udf2 = Nothing,
      organizationId = Nothing,
      deviceToken = Nothing,
      description = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }

defaultAdmin :: Person.Person
defaultAdmin =
  defaultDriver
    { Person.id = Id "admin",
      Person.firstName = "Admin",
      Person.lastName = Just "Adminson",
      Person.role = Person.ADMIN,
      Person.email = Just "adminson@cool-admins.com"
    }
