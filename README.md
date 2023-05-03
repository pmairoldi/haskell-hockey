# haskell-hockey

background task to parse nhl.com

## Build

```bash
stack build
```

## Run

### Worker

```bash
stack exec hockey-worker
```

### Server

```bash
stack exec hockey-web
```

### Debug Results API

```
import Data.Maybe
:l src/Hockey/Processing.hs
results <- fmap fromJust $  getResults (dateFromComponents 2023 05 07) (dateFromComponents 2023 05 07)
let games = (combineGameDates (dates results))
let game = games !! 0
T.status game
```
