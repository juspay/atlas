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

Module      :  Product.Profile
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Profile where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.Storage.Esqueleto (runTransaction)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.Profile as Profile
import Types.Error
import Utils.Common (fromMaybeM, withFlowHandlerAPI)

getPersonDetails :: Id Person.Person -> FlowHandler Profile.ProfileRes
getPersonDetails personId = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decPerson <- decrypt person
  return $ Person.makePersonAPIEntity decPerson

updatePerson :: Id Person.Person -> Profile.UpdateProfileReq -> FlowHandler APISuccess.APISuccess
updatePerson personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  runTransaction $
    QPerson.updatePersonalInfo
      personId
      (req.firstName)
      (req.middleName)
      (req.lastName)
      (req.deviceToken)
  pure APISuccess.Success
