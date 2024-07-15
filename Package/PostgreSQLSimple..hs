module PostgreSQLSimple where

import Data.Pool
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import Data.Time
import Test.Hspec
import Data.ByteString

-- https://edu.anarcho-copy.org/Programming%20Languages/Haskell/Practical%20Web%20Development%20with%20Haskell.pdf
-- ebook p102

data Config = Config
  { configUrl :: ByteString
  , configStripeCount :: Int
  , configMaxOpenConnPerStripe :: Int
  , configIdleConnTimeout :: NominalDiffTime
  }

withPool :: Config -> (Pool Connection -> IO a) -> IO a
withPool config action = bracket initPool cleanPool action
  where initPool = createPool openConn closeConn
                   (configStripeCount config)
                   (configIdleConnTimeout config)
                   (configMaxOpenConnPerStripe config)
        cleanPool = destroyAllResources
        openConn = connectPostgreSQL (configUrl config)
        closeConn = close

withState :: Config -> (Pool Connection -> IO a) -> IO a
withState config action = withPool config $ \state -> migrate state >> action state

openAndClose :: IO ()
openAndClose = do
  conn <- connectPostgreSQL "postgresql://localhost/PostgreSQLSimpleTest"
  close conn

-- runMigrations :: Bool -> Connection -> [MigrationCommand] -> IO (MigrationResult String)
migrate :: Pool Connection -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransation conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> return ()
  where
    cmds = [ MigrationInitialization
           , MigrationDirectory "Data/PostgreSQL"
           ]

spec :: SpecWith ()
spec = do
  it "connection" $ do
    openAndClose

main :: IO ()
main = hspec spec
