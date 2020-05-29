module Database.Migrate where

import           MyPrelude

import           Database.Beam
import           Database.Beam.Migrate

import           Database

migrate :: WithDB r m => m ()
migrate = void $ runDB $
  runMigrationSteps 0 Nothing migrationSteps (\_n _comment -> executeMigration runNoReturn)
