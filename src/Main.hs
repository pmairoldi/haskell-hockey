import Hockey.Database
import Hockey.Processing
import Hockey.Formatting

database :: Database
database = (Database Postgres "pierremarcairoldi" "localhost" "pierremarcairoldi" "" 5432 10)
-- database = (Database SQLite "" "" "" "" 0 10)

begin :: Day
begin = (dateFromComponents 2014 4 6)

end :: Day
end = (dateFromComponents 2014 4 7)

main :: IO ()
main = do
    migrate database

    games <- getGames begin end
    database `process` (upsertMany games)

    videos <- getVideos games
    database `process` (upsertMany videos)

    return ()
