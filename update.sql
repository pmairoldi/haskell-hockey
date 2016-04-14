UPDATE "public"."playoff_seed" SET "home_id"='wsh', "away_id"='phi' WHERE "year"='2015' AND "round"='1' AND "conference"='e' AND "series"='3' RETURNING "id", "year", "conference", "round", "home_id", "away_id", "series", "home_seed","away_seed";

UPDATE "public"."playoff_seed" SET "home_id"='pit', "away_id"='nyr' WHERE "year"='2015' AND "round"='1' AND "conference"='e' AND "series"='4' RETURNING "id", "year", "conference", "round", "home_id", "away_id", "series", "home_seed","away_seed";

UPDATE "public"."playoff_seed" SET "home_id"='fla', "away_id"='nyi' WHERE "year"='2015' AND "round"='1' AND "conference"='e' AND "series"='1' RETURNING "id", "year", "conference", "round", "home_id", "away_id", "series", "home_seed","away_seed";

UPDATE "public"."playoff_seed" SET "home_id"='tbl', "away_id"='det' WHERE "year"='2015' AND "round"='1' AND "conference"='e' AND "series"='2' RETURNING "id", "year", "conference", "round", "home_id", "away_id", "series", "home_seed","away_seed";

UPDATE "public"."playoff_seed" SET "home_id"='dal', "away_id"='min' WHERE "year"='2015' AND "round"='1' AND "conference"='w' AND "series"='1' RETURNING "id", "year", "conference", "round", "home_id", "away_id", "series", "home_seed","away_seed";

UPDATE "public"."playoff_seed" SET "home_id"='stl', "away_id"='chi' WHERE "year"='2015' AND "round"='1' AND "conference"='w' AND "series"='2' RETURNING "id", "year", "conference", "round", "home_id", "away_id", "series", "home_seed","away_seed";

UPDATE "public"."playoff_seed" SET "home_id"='ana', "away_id"='nsh' WHERE "year"='2015' AND "round"='1' AND "conference"='w' AND "series"='3' RETURNING "id", "year", "conference", "round", "home_id", "away_id", "series", "home_seed","away_seed";

UPDATE "public"."playoff_seed" SET "home_id"='lak', "away_id"='sjs' WHERE "year"='2015' AND "round"='1' AND "conference"='w' AND "series"='4' RETURNING "id", "year", "conference", "round", "home_id", "away_id", "series", "home_seed","away_seed";
