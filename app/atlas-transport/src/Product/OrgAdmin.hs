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

Module      :  Product.OrgAdmin
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.OrgAdmin where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Storage.Esqueleto as Esq
import Data.Maybe
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.OrgAdmin as API
import Types.Error
import Utils.Common

getProfile :: SP.Person -> FlowHandler API.OrgAdminProfileRes
getProfile admin = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  org <- QOrg.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  decAdmin <- decrypt admin
  let personAPIEntity = SP.makePersonAPIEntity decAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

updateProfile :: SP.Person -> API.UpdateOrgAdminProfileReq -> FlowHandler API.UpdateOrgAdminProfileRes
updateProfile admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
      updAdmin =
        admin{firstName = fromMaybe admin.firstName req.firstName,
              middleName = req.middleName <|> admin.middleName,
              lastName = req.lastName <|> admin.lastName,
              deviceToken = req.deviceToken <|> admin.deviceToken
             }
  Esq.runTransaction $
    QPerson.updatePersonRec updAdmin.id updAdmin
  org <- QOrg.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  decUpdAdmin <- decrypt updAdmin
  let personAPIEntity = SP.makePersonAPIEntity decUpdAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

makeOrgAdminProfileRes :: SP.PersonAPIEntity -> Org.OrganizationAPIEntity -> API.OrgAdminProfileRes
makeOrgAdminProfileRes SP.PersonAPIEntity {..} org =
  API.OrgAdminProfileRes
    { organization = org,
      ..
    }
