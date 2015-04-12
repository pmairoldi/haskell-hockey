module Hockey.Teams (
    teams,
    ducks,
    bruins,
    sabres,
    flames,
    hurricanes,
    blackhawks,
    avalanche,
    blueJackets,
    stars,
    redWings,
    oilers,
    panthers,
    kings,
    wild,
    canadiens,
    predators,
    devils,
    islanders,
    rangers,
    senators,
    flyers,
    coyotes,
    penguins,
    sharks,
    blues,
    lightning,
    mapleLeafs,
    canucks,
    capitals,
    jets,
    thrashers
)

where

import Hockey.Types

ducks :: Team
ducks = Team "ana" "Anaheim" "Ducks"

bruins :: Team
bruins = Team "bos" "Boston" "Bruins"

sabres :: Team
sabres = Team "buf" "Buffalo" "Sabres"

flames :: Team
flames = Team "cgy" "Calgary" "Flames"

hurricanes :: Team
hurricanes = Team "car" "Carolina" "Hurricanes"

blackhawks :: Team
blackhawks = Team "chi" "Chicago" "Blackhawks"

avalanche :: Team
avalanche = Team "col" "Colorado" "Avalanche"

blueJackets :: Team
blueJackets = Team "cbj" "Columbus" "Blue Jackets"

stars :: Team
stars = Team "dal" "Dallas" "Stars"

redWings :: Team
redWings = Team "det" "Detroit" "Red Wings"

oilers :: Team
oilers = Team "edm" "Edmonton" "Oilers"

panthers :: Team
panthers = Team "fla" "Florida" "Panthers"

kings :: Team
kings = Team "lak" "Los Angeles" "Kings"

wild :: Team
wild = Team "min" "Minnesota" "Wild"

canadiens :: Team
canadiens = Team "mtl" "Montréal" "Canadiens"

predators :: Team
predators = Team "nsh" "Nashville" "Predators"

devils :: Team
devils = Team "njd" "New Jersey" "Devils"

islanders :: Team
islanders = Team "nyi" "New York" "Islanders"

rangers :: Team
rangers = Team "nyr" "New York" "Rangers"

senators :: Team
senators = Team "ott" "Ottawa" "Senators"

flyers :: Team
flyers = Team "phi" "Philadelphia" "Flyers"

coyotes :: Team
coyotes = Team "phx" "Arizona" "Coyotes"

penguins :: Team
penguins = Team "pit" "Pittsburgh" "Penguins"

sharks :: Team
sharks = Team "sjs" "San Jose" "Sharks"

blues :: Team
blues = Team "stl" "St. Louis" "Blues"

lightning :: Team
lightning = Team "tbl" "Tampa Bay" "Lightning"

mapleLeafs :: Team
mapleLeafs = Team "tor" "Toronto" "Maple Leafs"

canucks :: Team
canucks = Team "van" "Vancouver" "Canucks"

capitals :: Team
capitals = Team "wsh" "Washington" "Capitals"

jets :: Team
jets = Team "wpg" "Winnipeg" "Jets"

thrashers :: Team
thrashers = Team "atl" "Atlanta" "Thrashers"

teams :: [Team]
teams = [ducks, bruins, sabres, flames, hurricanes, blackhawks, avalanche, blueJackets, stars, redWings, oilers, panthers, kings, wild, canadiens, predators, devils, islanders, rangers, senators, flyers, coyotes, penguins, sharks, blues, lightning, mapleLeafs, canucks, capitals, jets, thrashers]
