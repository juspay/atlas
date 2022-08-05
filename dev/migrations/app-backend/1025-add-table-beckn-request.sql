CREATE TABLE atlas_app.atlas_request (
  id character varying(36) PRIMARY KEY,
  atlas_request text NOT NULL,
  signature_header varchar(255) NOT NULL,
  time_stamp timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);