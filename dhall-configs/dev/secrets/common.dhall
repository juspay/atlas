let topSecret = ./top-secret.dhall
let globalCommon = ../../generic/common.dhall

let exotelCfg =
  { apiKey = "xxxxxxx"
  , apiToken = "xxxxxxx"
  , sid = "xxxxxxx"
  , callerId = "xxxxxxx"
  }

in

{ smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
, exotelCfg = exotelCfg
, googleMapsKey = topSecret.googleMapsKey
}
