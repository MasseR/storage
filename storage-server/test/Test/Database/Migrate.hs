module Test.Database.Migrate where

import           Test.Hspec

import           Database
import           Database.Beam
import           Database.Object
import           Database.SQLite.Simple (Connection, withConnection)

import           Control.Lens           ((^.))

-- System under test
import           Database.Migrate

spec :: Spec
spec = around setup $ describe "Database.Migrate" $
  it "should run the migrations" $ \db -> do
    got <- run db $ do
      migrate
      runDB $ runSelectReturningList $ select $ all_ (storage ^. objects)
    got `shouldBe` []
  where
    run = flip runReaderT
    setup :: (Connection -> IO a) -> IO a
    setup = withConnection ":memory:"
