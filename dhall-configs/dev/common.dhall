let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

let mockRegistryUrl = "http://localhost:8020/"
let nsdlRegistryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in/"
let atlasOneRegistryUrl = "https://beckn-one.succinct.in/subscribers"


in { defaultPoolConfig = globalCommon.defaultPoolConfig
   , smsSessionConfig = globalCommon.smsSessionConfig
   , autoMigrate = globalCommon.autoMigrate
   , loggerConfig = globalCommon.loggerConfig // { logToFile = True, logRawSql = True, prettyPrinting = True }
   , LogLevel = globalCommon.LogLevel
   , ExotelCfg = globalCommon.ExotelCfg
   , exotelCfg = sec.exotelCfg
   , signatureExpiry = globalCommon.signatureExpiry
   , httpClientOptions = globalCommon.httpClientOptions

   , smsUserName = sec.smsUserName
   , smsPassword = sec.smsPassword
   , passetto = { _1 = "localhost", _2 = 8021 }
   , fcmJsonPath = Some "dummy-fcm.json"
   , googleMapsUrl = "https://maps.googleapis.com/maps/api/"
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "http://localhost:4545/"
   , graphhopperUrl = "https://api.sandbox.beckn.juspay.in/map/grphr/"
   , registryUrl = mockRegistryUrl
   , authServiceUrl = "http://localhost:8013/"
   }
