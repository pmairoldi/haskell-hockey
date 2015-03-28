CREATE TABLE IF NOT EXISTS team (
"id" serial,
"team_id" char(3) NOT NULL,
"location" text NOT NULL,
"name" text NOT NULL,
PRIMARY KEY ("id"), 
UNIQUE ("team_id")
);
 
CREATE OR REPLACE RULE ignore_duplicate_inserts_on_teams AS ON INSERT TO teams
WHERE (EXISTS (SELECT 1 FROM teams WHERE team_id = new.team_id))
DO INSTEAD NOTHING;
   
INSERT INTO teams(team_id, location, name)
VALUES
	('ana', 'Anaheim', 'Ducks'),
	('bos', 'Boston', 'Bruins'),
	('buf', 'Buffalo', 'Sabres'),
	('cgy', 'Calgary', 'Flames'),
	('car', 'Carolina', 'Hurricanes'),
	('chi', 'Chicago', 'Blackhawks'),
	('col', 'Colorado', 'Avalanche'),
	('cbj', 'Columbus', 'Blue Jackets'),
	('dal', 'Dallas', 'Stars'),
	('det', 'Detroit', 'Red Wings'),
	('edm', 'Edmonton', 'Oilers'),
	('fla', 'Florida', 'Panthers'),
	('lak', 'Los Angeles', 'Kings'),
	('min', 'Minnesota', 'Wild'),
	('mtl', 'Montréal', 'Canadiens'),
	('nsh', 'Nashville', 'Predators'),
	('njd', 'New Jersey', 'Devils'),
	('nyi', 'New York', 'Islanders'),
	('nyr', 'New York', 'Rangers'),
	('ott', 'Ottawa', 'Senators'),
	('phi', 'Philadelphia', 'Flyers'),
	('phx', 'Phoenix', 'Coyotes'),
	('pit', 'Pittsburgh', 'Penguins'),
	('sjs', 'San Jose', 'Sharks'),
	('stl', 'St. Louis', 'Blues'),
	('tbl', 'Tampa Bay', 'Lightning'),
	('tor', 'Toronto', 'Maple Leafs'),
	('van', 'Vancouver', 'Canucks'),
	('wsh', 'Washington', 'Capitals'),
	('wpg', 'Winnipeg', 'Jets'),
	('atl', 'Atlanta', 'Thrashers');