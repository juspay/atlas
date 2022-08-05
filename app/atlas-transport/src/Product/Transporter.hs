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

Module      :  Product.Transporter
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Transporter where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import qualified Domain.Types.Organization as SO
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import Types.API.Transporter
import Types.Error
import Utils.Common

updateTransporter :: SP.Person -> Id SO.Organization -> UpdateTransporterReq -> FlowHandler UpdateTransporterRes
updateTransporter admin orgId req = withFlowHandlerAPI $ do
  unless (Just orgId == admin.organizationId) $ throwError AccessDenied
  runRequestValidation validateUpdateTransporterReq req
  org <-
    QO.findById orgId
      >>= fromMaybeM (OrgDoesNotExist orgId.getId)
  let updOrg =
        org{SO.name = fromMaybe (org.name) (req.name),
            SO.description = (req.description) <|> (org.description),
            SO.enabled = fromMaybe (org.enabled) (req.enabled)
           }
  Esq.runTransaction $ QO.updateOrganizationRec updOrg
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateTransporter : ") (show updOrg)
  return $ SO.makeOrganizationAPIEntity updOrg

getTransporter :: Id SP.Person -> FlowHandler TransporterRec
getTransporter personId = withFlowHandlerAPI $ do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  case person.organizationId of
    Just orgId -> TransporterRec . SO.makeOrganizationAPIEntity <$> (QO.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId))
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
