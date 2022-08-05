let common = ./common.dhall
let main = ./public-transport-bap.dhall

let juspayGatewayUrl = "http://localhost:8015/v1"

let kafkaConsumerCfgs =
  { publicTransportSearch = { brokers = ["localhost:29092"],
                              groupId = "publicTransportSearchGroup",
                              timeoutMilliseconds = +10000}
  }

in
{ esqDBCfg = main.esqDBCfg
, migrationPath = main.migrationPath
, autoMigrate = main.autoMigrate
, port = +8024
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/public-transport-search-consumer.log"}
, graceTerminationPeriod = +90
, bapId = main.selfId
, bapURI = main.selfURI
, gatewayUrl = juspayGatewayUrl
, httpClientOptions = main.httpClientOptions
, authServiceUrl = main.authServiceUrl
, authEntity = main.authEntity
, kafkaConsumerCfgs = kafkaConsumerCfgs
}
