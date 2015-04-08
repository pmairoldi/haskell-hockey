import Hockey.Database
import Hockey.Processing
import Hockey.Formatting

database :: Database
-- database = (Database SQLite "test.sqlite" "" "" "" 0 10)
database = (Database Postgres "pierremarcairoldi" "localhost" "pierremarcairoldi" "" 5432 10)

main :: IO ()
main = do
    migrate database

    processGames database (dateFromComponents 2014 4 6) (dateFromComponents 2014 4 7)

    return ()
