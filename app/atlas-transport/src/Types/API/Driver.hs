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

Module      :  Types.API.Driver

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Types.API.Driver
  ( DriverInformationRes (..),
    ListDriverRes (..),
    DriverEntityRes (..),
    LinkVehicleRes,
    OnboardDriverReq (..),
    OnboardDriverRes (..),
    CreatePerson (..),
    CreateVehicle (..),
    UpdateDriverReq (..),
    UpdateDriverRes,
    validateCreatePerson,
    validateCreateVehicle,
    validateOnboardDriverReq,
    validateUpdateDriverReq,
  )
where

import Beckn.External.FCM.Types (FCMRecipientToken)
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import qualified Domain.Types.Organization as Organization
import Domain.Types.Person (Person, PersonAPIEntity)
import Domain.Types.Vehicle (VehicleAPIEntity)
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude hiding (id)

data DriverInformationRes = DriverInformationRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    optForRental :: Bool,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    organization :: Organization.OrganizationAPIEntity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ListDriverRes = ListDriverRes
  {list :: [DriverEntityRes]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverEntityRes = DriverEntityRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    optForRental :: Bool,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    registeredAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type LinkVehicleRes = APISuccess

-- Create Person request and response
data OnboardDriverReq = OnboardDriverReq
  { person :: CreatePerson,
    vehicle :: CreateVehicle
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateOnboardDriverReq :: Validate OnboardDriverReq
validateOnboardDriverReq OnboardDriverReq {..} =
  sequenceA_
    [ validateObject "person" person validateCreatePerson,
      validateObject "vehicle" vehicle validateCreateVehicle
    ]

data CreatePerson = CreatePerson
  { firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

validateCreatePerson :: Validate CreatePerson
validateCreatePerson CreatePerson {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe $ NotEmpty `And` P.name,
      validateField "lastName" lastName $ InMaybe $ NotEmpty `And` P.name,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

data CreateVehicle = CreateVehicle
  { category :: Veh.Category,
    model :: Text,
    variant :: Veh.Variant,
    color :: Text,
    registrationNo :: Text,
    capacity :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

validateCreateVehicle :: Validate CreateVehicle
validateCreateVehicle CreateVehicle {..} =
  sequenceA_
    [ validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit),
      validateField "model" model $
        NotEmpty `And` star P.latinOrSpace,
      validateField "color" color $ NotEmpty `And` P.name
    ]

newtype OnboardDriverRes = OnboardDriverRes
  {driver :: PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateDriverReq = UpdateDriverReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken,
    canDowngradeToSedan :: Maybe Bool,
    canDowngradeToHatchback :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateUpdateDriverReq :: Validate UpdateDriverReq
validateUpdateDriverReq UpdateDriverReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ InMaybe $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe $ NotEmpty `And` P.name,
      validateField "lastName" lastName $ InMaybe $ NotEmpty `And` P.name
    ]

type UpdateDriverRes = DriverInformationRes
