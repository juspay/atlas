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

Module      :  Tools.Streaming.Kafka.Environment

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Tools.Streaming.Kafka.Environment
  ( module Tools.Streaming.Kafka.Environment,
    module Reexport,
  )
where

import Beckn.Streaming.Kafka.Producer.Types as Reexport
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Types as Reexport
import EulerHS.Prelude

newtype BAPKafkaEnvs = BAPKafkaEnvs
  { businessEventEnv :: KafkaBEEnv
  }
  deriving (Generic)

buildBAPKafkaEnvs :: IO BAPKafkaEnvs
buildBAPKafkaEnvs = do
  businessEventEnv <- buildKafkaBEEnv "BAP"

  return $ BAPKafkaEnvs {..}
