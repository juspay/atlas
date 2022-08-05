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

Module      :  Storage.Queries.Products
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.Products where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Products
import Storage.Tabular.Products

create :: Products -> SqlDB ()
create = create'

findAllById :: Transactionable m => [Id Products] -> m [Products]
findAllById ids =
  Esq.findAll $ do
    products <- from $ table @ProductsT
    where_ $
      products ^. ProductsTId `in_` valList (toKey <$> ids)
    return products

findById :: Transactionable m => Id Products -> m (Maybe Products)
findById = Esq.findById

findByName :: Transactionable m => Text -> m (Maybe Products)
findByName name =
  Esq.findOne $ do
    products <- from $ table @ProductsT
    where_ $
      products ^. ProductsName ==. val name
    return products
