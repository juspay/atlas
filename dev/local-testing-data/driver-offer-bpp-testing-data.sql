INSERT INTO atlas_driver_offer_bpp.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, description, mobile_number, mobile_country_code, from_time, to_time, api_key, head_count, created_at, updated_at, info, unique_key_id) VALUES
	('1926d40f-1223-4eb2-ba5d-7983bde2fd02', 'Juspay Gateway', 'JUSPAY.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.213874+00', '2022-04-12 15:15:42.213874+00', NULL, 'FIXME'),
	('0d7016d0-f9cd-4f9f-886f-bc4cbd6a86e5', 'NSDL Gateway', 'NSDL', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.215812+00', '2022-04-12 15:15:42.215812+00', NULL, 'FIXME'),
	('505e4651-5340-4836-81a7-045394ba6dc3', 'Mobility BAP', 'JUSPAY.MOBILITY.APP.UAT.1', NULL, 'APPROVED', 'APP', 'MOBILITY', true, true, NULL, '9777777777', '+91', NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.21825+00', '2022-04-12 15:15:42.21825+00', NULL, 'FIXME'),
	('239ee68b-0818-4dba-ad31-032fe809cf71', 'NSDL Gateway', 'NSDL.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.21902+00', '2022-04-12 15:15:42.21902+00', NULL, 'FIXME'),
	('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'Driver-Offer-Provider #1', 'JUSPAY.MOBILITY.PROVIDER.UAT.3', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', false, true, NULL, '9888888888', '+91', NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.216602+00', '2022-04-12 15:15:42.216602+00', NULL, 'juspay-mobility-bpp-1-key'),
	('7f7896dd-787e-4a0b-8675-c3f6fe93aa9e', 'Driver-Offer-Provider #2', 'JUSPAY.MOBILITY.PROVIDER.UAT.4', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, '9888888777', '+91', NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.216602+00', '2022-04-12 15:15:42.216602+00', NULL, 'juspay-mobility-bpp-1-key');

INSERT INTO atlas_driver_offer_bpp.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, is_new, udf1, udf2, organization_id, device_token, description, created_at, updated_at, rating) VALUES
	('6bc4bc84-2c43-425d-8853-22f47driver1', 'Suresh', 'aka', 'Dhinesh', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+91', NULL, true, '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.21973+00', '2022-04-12 15:15:42.21973+00', NULL),
	('6bc4bc84-2c43-425d-8853-22f47driver2', 'Bob', 'aka', 'Dhinesh', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+94', NULL, true, '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.221002+00', '2022-04-12 15:15:42.221002+00', NULL),
	('3b2af603-c6a3-404e-9859-276085fc6e65', 'Thomas', 'aka', NULL, NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, '+91', NULL, true, 'f1b84838-5cb5-4eb1-8f4b-0dd0c4a2-suv', 'VEHICLE', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, NULL, '2022-04-12 15:15:42.222142+00', '2022-04-12 15:15:42.222142+00', NULL),
	('ec34eede-5a3e-4a41-89d4-7290a0d7a629', NULL, NULL, NULL, NULL, 'ADMIN', 'UNKNOWN', 'MOBILENUMBER', NULL, NULL, '0.1.0|1|8hdNzNRjeq0j7QRZoKzT2iMjoKkQJzwLoUeOEF9edh1wmRer7NbUaUT3foUI1wG2gZMjpB8vDuUKUDG+aQ==', '\xba0f72d2d550eb699914591dab2eadeb8fc83dd1eda6bde7cc3f68d7138f14fe', '+91', NULL, false, NULL, NULL, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.223341+00', '2022-04-12 15:15:42.223341+00', NULL),
	('a30193df-4f7c-440f-bada-4d46c396d7d0', NULL, NULL, NULL, NULL, 'ADMIN', 'UNKNOWN', 'MOBILENUMBER', NULL, NULL, '0.1.0|0|BEr9F11LIq8SIoxEwzGp3sD3QWLhty3XYOvxyI5r6H90GIh8BUjXiTKsmj+F15FFICiUhSw4GB8yRj7wtA==', '\x0b0c9417ddba512efba45e716b6f6e7abfbb4307ac4dd8204a58936acfdbac37', '+91', NULL, false, NULL, NULL, 'e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, NULL, '2022-04-12 15:15:42.22423+00', '2022-04-12 15:15:42.22423+00', NULL),
	('001093df-4f7c-440f-b-furthest_driver', NULL, NULL, NULL, NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, NULL, NULL, false, '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.225349+00', '2022-04-12 15:15:42.225349+00', NULL),
	('002093df-4f7c-440f-ba-closest_driver', NULL, NULL, NULL, NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, NULL, NULL, false, '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.225349+00', '2022-04-12 15:15:42.225349+00', NULL),
	('003093df-4f7c-440f-bada-other_driver', NULL, NULL, NULL, NULL, 'DRIVER', 'FEMALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, NULL, NULL, false, '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, NULL, '2022-04-12 15:15:42.225349+00', '2022-04-12 15:15:42.225349+00', NULL),
	('003093df-4f7c-440f-bada-4-suv_driver', NULL, NULL, NULL, NULL, 'DRIVER', 'FEMALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, NULL, NULL, false, 'f1b84838-5cb5-4eb1-8f4b-0dd0c4a2-suv', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.225349+00', '2022-04-12 15:15:42.225349+00', NULL),
	('003093df-4f7c-440f-bada-sedan_driver', NULL, NULL, NULL, NULL, 'DRIVER', 'FEMALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, NULL, NULL, false, 'f1b84838-5cb5-4eb1-8f4b-0dd0c4-sedan', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.225349+00', '2022-04-12 15:15:42.225349+00', NULL),
	('003093df-4f7c-440f--hatchback_driver', NULL, NULL, NULL, NULL, 'DRIVER', 'FEMALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, NULL, NULL, false, 'f1b84838-5cb5-4eb1-8f4b-0d-hatchback', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.225349+00', '2022-04-12 15:15:42.225349+00', NULL),
	('001b93df-4f7c-440f-bada-4d46c396d7d0', 'Some', 'Cool', 'Driver', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+92', '001', false, '001cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.291949+00', '2022-04-12 15:15:42.291949+00', NULL),
	('002b93df-4f7c-440f-bada-4d46c396d7d0', 'Another', 'Cool', 'Driver', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+93', '002', false, '002cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, '2022-04-12 15:15:42.295658+00', '2022-04-12 15:15:42.295658+00', NULL);
INSERT INTO atlas_driver_offer_bpp.driver_information (driver_id, active, on_ride, created_at, updated_at, enabled) VALUES
	('6bc4bc84-2c43-425d-8853-22f47driver1', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('6bc4bc84-2c43-425d-8853-22f47driver2', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('3b2af603-c6a3-404e-9859-276085fc6e65', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('001093df-4f7c-440f-b-furthest_driver', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('002093df-4f7c-440f-ba-closest_driver', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('003093df-4f7c-440f-bada-other_driver', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('003093df-4f7c-440f-bada-4-suv_driver', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('003093df-4f7c-440f-bada-sedan_driver', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('003093df-4f7c-440f--hatchback_driver', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
	('001b93df-4f7c-440f-bada-4d46c396d7d0', false, false, '2022-04-12 15:15:42.297156+00', '2022-04-12 15:15:42.297156+00', true),
	('002b93df-4f7c-440f-bada-4d46c396d7d0', false, false, '2022-04-12 15:15:42.298253+00', '2022-04-12 15:15:42.298253+00', true);
INSERT INTO atlas_driver_offer_bpp.driver_location (driver_id, lat, lon, point, created_at, updated_at) VALUES
	('6bc4bc84-2c43-425d-8853-22f47driver1', 10.0739, 76.2733, '0101000020E6100000CC7F48BF7D1153404B598638D6252440', '2022-04-12 15:15:42.279179+00', '2022-04-12 15:15:42.279179+00'),
	('6bc4bc84-2c43-425d-8853-22f47driver2', 10.0741, 76.2733, '0101000020E6100000CC7F48BF7D1153406744696FF0252440', '2022-04-12 15:15:42.280142+00', '2022-04-12 15:15:42.280142+00'),
	('3b2af603-c6a3-404e-9859-276085fc6e65', 10.0739, 76.2733, '0101000020E6100000CC7F48BF7D1153404B598638D6252440', '2022-04-12 15:15:42.27825+00', '2022-04-12 15:15:42.27825+00'),
	('001093df-4f7c-440f-b-furthest_driver', 13.005432, 77.59336, '0101000020E61000004BB0389CF965534029B4ACFBC7022A40', '2022-04-12 15:15:42.27627+00', '2022-04-12 15:15:42.27627+00'),
	('002093df-4f7c-440f-ba-closest_driver', 13.005432, 77.59336, '0101000020E6100000A471A8DF8566534023D74D29AFFD2940', '2022-04-12 15:15:42.27627+00', '2022-04-12 15:15:42.27627+00'),
	('003093df-4f7c-440f-bada-other_driver', 13.005432, 77.59336, '0101000020E61000008C2FDAE38566534065C6DB4AAFFD2940', '2022-04-12 15:15:42.27627+00', '2022-04-12 15:15:42.27627+00'),
	('003093df-4f7c-440f-bada-4-suv_driver', 13.005432, 77.59336, '0101000020E61000004BB0389CF9655340043DD4B6611C2A40', '2022-04-12 15:15:42.27627+00', '2022-04-12 15:15:42.27627+00'),
	('003093df-4f7c-440f-bada-sedan_driver', 13.005432, 77.59336, '0101000020E61000004BB0389CF965534091D442C9E41C2A40', '2022-04-12 15:15:42.27627+00', '2022-04-12 15:15:42.27627+00'),
	('003093df-4f7c-440f--hatchback_driver', 13.005432, 77.59336, '0101000020E61000004BB0389CF96553401E6CB1DB671D2A40', '2022-04-12 15:15:42.27627+00', '2022-04-12 15:15:42.27627+00'),
	('001b93df-4f7c-440f-bada-4d46c396d7d0', 10, 76.2733, '0101000020E6100000CC7F48BF7D1153400000000000002440', '2022-04-12 15:15:42.290953+00', '2022-04-12 15:15:42.290953+00'),
	('002b93df-4f7c-440f-bada-4d46c396d7d0', 10, 76.2733, '0101000020E6100000CC7F48BF7D1153400000000000002440', '2022-04-12 15:15:42.294728+00', '2022-04-12 15:15:42.294728+00');
INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id, idle_since) VALUES
	('6bc4bc84-2c43-425d-8853-22f47driver1', '2022-04-12 15:15:42.283174+00'),
	('6bc4bc84-2c43-425d-8853-22f47driver2', '2022-04-12 15:15:42.283174+00'),
	('3b2af603-c6a3-404e-9859-276085fc6e65', '2022-04-12 15:15:42.283174+00'),
	('001093df-4f7c-440f-b-furthest_driver', '2022-04-12 15:15:42.283174+00'),
	('002093df-4f7c-440f-ba-closest_driver', '2022-04-12 15:15:42.283174+00'),
	('003093df-4f7c-440f-bada-other_driver', '2022-04-12 15:15:42.283174+00'),
	('003093df-4f7c-440f-bada-4-suv_driver', '2022-04-12 15:15:42.283174+00'),
	('003093df-4f7c-440f-bada-sedan_driver', '2022-04-12 15:15:42.283174+00'),
	('003093df-4f7c-440f--hatchback_driver', '2022-04-12 15:15:42.283174+00'),
	('001b93df-4f7c-440f-bada-4d46c396d7d0', '2022-04-12 15:15:42.299365+00'),
	('002b93df-4f7c-440f-bada-4d46c396d7d0', '2022-04-12 15:15:42.300253+00');

INSERT INTO atlas_driver_offer_bpp.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, info, created_at, updated_at) VALUES
	('772453e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '3249', 'ea37f941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, 'ec34eede-5a3e-4a41-89d4-7290a0d7a629', 'USER                                ', NULL, '2022-04-12 15:15:42.231116+00', '2022-04-12 15:15:42.231116+00'),
	('c38562c2-3d58-4b08-8496-drivertoken1', 'SMS', 'OTP', '3249', 'ca05cf3c-c88b-4a2f-8874-drivertoken1', true, 3, 365, 3, '6bc4bc84-2c43-425d-8853-22f47driver1', 'USER                                ', NULL, '2022-04-12 15:15:42.232189+00', '2022-04-12 15:15:42.232189+00'),
	('c38562c2-3d58-4b08-8496-drivertoken2', 'SMS', 'OTP', '3249', 'ca05cf3c-c88b-4a2f-8874-drivertoken2', true, 3, 365, 3, '6bc4bc84-2c43-425d-8853-22f47driver2', 'USER                                ', NULL, '2022-04-12 15:15:42.232974+00', '2022-04-12 15:15:42.232974+00'),
	('001d53e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '1233', '001df941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, '001b93df-4f7c-440f-bada-4d46c396d7d0', 'USER                                ', NULL, '2022-04-12 15:15:42.292899+00', '2022-04-12 15:15:42.292899+00'),
	('002d53e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '1234', '002df941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, '002b93df-4f7c-440f-bada-4d46c396d7d0', 'USER                                ', NULL, '2022-04-12 15:15:42.296488+00', '2022-04-12 15:15:42.296488+00');

INSERT INTO atlas_driver_offer_bpp.fare_policy
  (id, organization_id, base_fare, night_shift_start, night_shift_end,
    night_shift_rate, created_at, updated_at) VALUES
  ('0991cec4-72d4-40f6-8ddd-c77a97c3b898', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 120, NULL, NULL, 1, now(), now()),
  ('002d53e2-d02b-494a-a4ac-ec1ea0027e18', '7f7896dd-787e-4a0b-8675-c3f6fe93aa9e', 120, NULL, NULL, 1, now(), now());

INSERT INTO atlas_driver_offer_bpp.fare_policy_per_extra_km_rate
  (id, organization_id, distance_range_start, fare) VALUES
  ('0991cec4-72d4-40f6-8ddd-c77a97c3b897', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 5000, 12),
  ('002d53e2-d02b-494a-a4ac-ec1ea0027e18', '7f7896dd-787e-4a0b-8675-c3f6fe93aa9e', 5000, 12);
INSERT INTO atlas_driver_offer_bpp.vehicle (id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, organization_id, created_at, updated_at) VALUES
	('0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', NULL, NULL, NULL, 'Model1', NULL, 'SUV', 'Black', NULL, '4810', NULL, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '2022-04-12 15:15:42.233691+00', '2022-04-12 15:15:42.233691+00'),
	('f1b84838-5cb5-4eb1-8f4b-0dd0c4a2-suv', NULL, NULL, NULL, 'Model2', NULL, 'SUV', 'Black', NULL, '5613', NULL, 'e1f37274-f0aa-4bb3-93a0-2476349487b7', '2022-04-12 15:15:42.234832+00', '2022-04-12 15:15:42.234832+00'),
	('f1b84838-5cb5-4eb1-8f4b-0dd0c4-sedan', NULL, NULL, NULL, 'Model2', NULL, 'SEDAN', 'Black', NULL, '5614', NULL, 'e1f37274-f0aa-4bb3-93a0-2476349487b7', '2022-04-12 15:15:42.234832+00', '2022-04-12 15:15:42.234832+00'),
	('f1b84838-5cb5-4eb1-8f4b-0d-hatchback', NULL, NULL, NULL, 'Model2', NULL, 'HATCHBACK', 'Black', NULL, '5615', NULL, 'e1f37274-f0aa-4bb3-93a0-2476349487b7', '2022-04-12 15:15:42.234832+00', '2022-04-12 15:15:42.234832+00'),
	('001cd0bc-b3a4-4c6c-811f-900ccf4dfb94', NULL, NULL, NULL, 'Model3', NULL, 'SUV', 'WHITE', NULL, '4277', NULL, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '2022-04-12 15:15:42.289998+00', '2022-04-12 15:15:42.289998+00'),
	('002cd0bc-b3a4-4c6c-811f-900ccf4dfb94', NULL, NULL, NULL, 'Model4', NULL, 'SUV', 'GREEN', NULL, '3211', NULL, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '2022-04-12 15:15:42.293771+00', '2022-04-12 15:15:42.293771+00');
