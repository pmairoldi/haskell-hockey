module Hockey.Teams (
    teamList,
    teamByAbbreviation,
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
    thrashers,
    kraken
)

where

import Hockey.Types
import Data.List

ducks :: Team
ducks = Team "ana" "Anaheim" "Ducks" "#c93f10"

bruins :: Team
bruins = Team "bos" "Boston" "Bruins" "#cd920e"

sabres :: Team
sabres = Team "buf" "Buffalo" "Sabres" "#001a39"

flames :: Team
flames = Team "cgy" "Calgary" "Flames" "#9f1327"

hurricanes :: Team
hurricanes = Team "car" "Carolina" "Hurricanes" "#9f1327"

blackhawks :: Team
blackhawks = Team "chi" "Chicago" "Blackhawks" "#9f0a27"

avalanche :: Team
avalanche = Team "col" "Colorado" "Avalanche" "#501f31"

blueJackets :: Team
blueJackets = Team "cbj" "Columbus" "Blue Jackets" "#001a39"

stars :: Team
stars = Team "dal" "Dallas" "Stars" "#023927"

redWings :: Team
redWings = Team "det" "Detroit" "Red Wings" "#9f0a27"

oilers :: Team
oilers = Team "edm" "Edmonton" "Oilers" "#001a39"

panthers :: Team
panthers = Team "fla" "Florida" "Panthers" "#001a39"

kings :: Team
kings = Team "lak" "Los Angeles" "Kings" "#1b0655"

wild :: Team
wild = Team "min" "Minnesota" "Wild" "#023927"

canadiens :: Team
canadiens = Team "mtl" "Montréal" "Canadiens" "#861426"

predators :: Team
predators = Team "nsh" "Nashville" "Predators" "#cd920e"

devils :: Team
devils = Team "njd" "New Jersey" "Devils" "#9f0a27"

islanders :: Team
islanders = Team "nyi" "New York" "Islanders" "#c93f10"

rangers :: Team
rangers = Team "nyr" "New York" "Rangers" "#002e85"

senators :: Team
senators = Team "ott" "Ottawa" "Senators" "#9f0a27"

flyers :: Team
flyers = Team "phi" "Philadelphia" "Flyers" "#c73816"

coyotes :: Team
coyotes = Team "ari" "Arizona" "Coyotes" "#681d29"

penguins :: Team
penguins = Team "pit" "Pittsburgh" "Penguins" "#a7a17e"

sharks :: Team
sharks = Team "sjs" "San Jose" "Sharks" "#005360"

blues :: Team
blues = Team "stl" "St. Louis" "Blues" "#002971"

lightning :: Team
lightning = Team "tbl" "Tampa Bay" "Lightning" "#001e50"

mapleLeafs :: Team
mapleLeafs = Team "tor" "Toronto" "Maple Leafs" "#001e50"

canucks :: Team
canucks = Team "van" "Vancouver" "Canucks" "#006a35"

capitals :: Team
capitals = Team "wsh" "Washington" "Capitals" "#9e0a26"

jets :: Team
jets = Team "wpg" "Winnipeg" "Jets" "#001e50"

goldenKnights :: Team
goldenKnights = Team "vgk" "Vegas" "Golden Knights" "#80724f"

kraken :: Team
kraken = Team "sea" "Seattle" "Kraken" "#9cdbd9"

-- Old Teams
thrashers :: Team
thrashers = Team "atl" "Atlanta" "Thrashers" "#001e50"

oldCoyotes :: Team
oldCoyotes = Team "phx" "Phoenix" "Coyotes" "#681d29"

teamList :: [Team]
teamList = [ducks, bruins, sabres, flames, hurricanes, blackhawks, avalanche, blueJackets, stars, redWings, oilers, panthers, kings, wild, canadiens, predators, devils, islanders, rangers, senators, flyers, coyotes, penguins, sharks, blues, lightning, mapleLeafs, canucks, capitals, jets, goldenKnights, thrashers, oldCoyotes]

teamByAbbreviation :: String -> Maybe Team 
teamByAbbreviation team = find (\x -> abr x == team) teamList 
