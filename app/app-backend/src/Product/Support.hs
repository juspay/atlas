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

Module      :  Product.Support
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Support (sendIssue) where

import qualified App.Types as App
import Beckn.Storage.Esqueleto (runTransaction)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Validation (runRequestValidation)
import qualified Domain.Types.Issue as DIssue
import Domain.Types.Person as Person
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (length)
import qualified Storage.Queries.Issues as Queries
import Types.API.Support as Support
import Utils.Common

sendIssue :: Id Person.Person -> Support.SendIssueReq -> App.FlowHandler Support.SendIssueRes
sendIssue personId request@SendIssueReq {..} = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  runRequestValidation validateSendIssueReq request
  let personIdTxt = getId personId
  issueId <- L.generateGUID
  utcNow <- getCurrentTime
  runTransaction $
    Queries.insertIssue (mkDBIssue issueId personIdTxt request utcNow)
  return APISuccess.Success

mkDBIssue :: Text -> Text -> Support.SendIssueReq -> UTCTime -> DIssue.Issue
mkDBIssue issueId customerId SendIssueReq {..} time =
  DIssue.Issue
    { id = Id issueId,
      customerId = Id customerId,
      rideBookingId = rideBookingId,
      contactEmail = contactEmail,
      reason = issue.reason,
      description = issue.description,
      createdAt = time,
      updatedAt = time
    }
