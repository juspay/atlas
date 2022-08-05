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

Module      :  Product.Vehicle
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.Vehicle where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Validation (runRequestValidation)
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle.Variant as Variant
import Environment
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Types.API.Vehicle as API
import Types.Error
import Utils.Common
import qualified Utils.Defaults as Default

createVehicle :: SP.Person -> CreateVehicleReq -> FlowHandler CreateVehicleRes
createVehicle admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation validateCreateVehicleReq req
  validateVehicle
  vehicle <- API.createVehicle req orgId
  Esq.runTransaction $ QV.create vehicle
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> createVehicle : ") (show vehicle)
  return . CreateVehicleRes $ SV.makeVehicleAPIEntity vehicle
  where
    validateVehicle = do
      mVehicle <- QV.findByRegistrationNo $ req.registrationNo
      when (isJust mVehicle) $
        throwError $ InvalidRequest "Registration number already exists."

listVehicles :: SP.Person -> Maybe Variant.Variant -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler ListVehicleRes
listVehicles admin variantM mbRegNum limitM offsetM = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  personList <- QP.findAllByOrgId [SP.DRIVER] orgId
  vehicleList <- QV.findAllByVariantRegNumOrgId variantM mbRegNum limit offset orgId
  respList <- buildVehicleRes personList `traverse` vehicleList
  return $ ListVehicleRes respList
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM

updateVehicle :: SP.Person -> Id SV.Vehicle -> UpdateVehicleReq -> FlowHandler UpdateVehicleRes
updateVehicle admin vehicleId req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation validateUpdateVehicleReq req
  vehicle <- QV.findByIdAndOrgId vehicleId orgId >>= fromMaybeM (VehicleDoesNotExist vehicleId.getId)
  let updatedVehicle =
        vehicle{variant = fromMaybe vehicle.variant req.variant,
                model = fromMaybe vehicle.model req.model,
                color = fromMaybe vehicle.color req.color,
                category = req.category <|> vehicle.category
               }
  Esq.runTransaction $ QV.updateVehicleRec updatedVehicle
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateVehicle : ") (show updatedVehicle)
  return $ SV.makeVehicleAPIEntity vehicle

deleteVehicle :: SP.Person -> Id SV.Vehicle -> FlowHandler DeleteVehicleRes
deleteVehicle admin vehicleId = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  vehicle <-
    QV.findById vehicleId
      >>= fromMaybeM (VehicleDoesNotExist vehicleId.getId)
  unless (vehicle.organizationId == orgId) $ throwError Unauthorized
  Esq.runTransaction $
    QV.deleteById vehicleId
  return Success

getVehicle :: Id SP.Person -> Maybe Text -> Maybe (Id SV.Vehicle) -> FlowHandler CreateVehicleRes
getVehicle personId registrationNoM vehicleIdM = withFlowHandlerAPI $ do
  user <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  vehicle <- case (registrationNoM, vehicleIdM) of
    (Nothing, Nothing) -> throwError $ InvalidRequest "You should pass registration number and vehicle id."
    _ ->
      QV.findByAnyOf registrationNoM vehicleIdM
        >>= fromMaybeM (VehicleDoesNotExist personId.getId)
  hasAccess user vehicle
  return . CreateVehicleRes $ SV.makeVehicleAPIEntity vehicle
  where
    hasAccess user vehicle =
      when (user.organizationId /= Just (vehicle.organizationId)) $
        throwError Unauthorized

addOrgId :: Id Org.Organization -> SV.Vehicle -> SV.Vehicle
addOrgId orgId vehicle = vehicle {SV.organizationId = orgId}

buildVehicleRes :: MonadFlow m => [SP.Person] -> SV.Vehicle -> m VehicleRes
buildVehicleRes personList vehicle = do
  let mdriver =
        find
          ( \person ->
              person.udf1 == Just (getId $ vehicle.id)
          )
          personList
  return
    VehicleRes
      { vehicle = SV.makeVehicleAPIEntity vehicle,
        driver = mkDriverObj <$> mdriver
      }

mkDriverObj :: SP.Person -> Driver
mkDriverObj person =
  Driver
    { id = getId $ person.id,
      firstName = person.firstName,
      middleName = person.middleName,
      lastName = person.lastName,
      rating = round <$> person.rating,
      organizationId = person.organizationId
    }
