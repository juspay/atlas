let common = ./common.dhall
let sec = ./secrets/mock-registry.dhall

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
  , connectSchemaName = "atlas_registry"
  }

in

{ port = +8020
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-registry.log"}
, esqDBCfg = esqDBCfg
, autoMigrate = True
, migrationPath = Some (env:MOCK_REGISTRY_MIGRATION_PATH as Text ? "dev/migrations/mock-registry")
}
