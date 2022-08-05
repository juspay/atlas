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

Module      :  Storage.Queries.Rating
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.Rating where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.Rating
import Domain.Types.Ride
import Storage.Tabular.Rating
import Utils.Common

create :: Rating -> SqlDB ()
create = Esq.create'

updateRatingValue :: Id Rating -> Id Person -> Int -> SqlDB ()
updateRatingValue ratingId driverId newRatingValue = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RatingRatingValue =. val newRatingValue,
        RatingUpdatedAt =. val now
      ]
    where_ $
      tbl ^. RatingTId ==. val (toKey ratingId)
        &&. tbl ^. RatingDriverId ==. val (toKey driverId)

findByRideId :: Transactionable m => Id Ride -> m (Maybe Rating)
findByRideId rideId =
  findOne $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingRideId ==. val (toKey rideId)
    return rating

findAllRatingsForPerson :: Transactionable m => Id Person -> m [Rating]
findAllRatingsForPerson driverId =
  findAll $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingDriverId ==. val (toKey driverId)
    return rating
