{-# LANGUAGE DataKinds #-}

module Actions (
      Disk, Target, Machine
    , Template(TemplateFromModel)
    , createDiskClone
    , exposeISCSI
    , createMachine
    , startMachine
    , validateEnvironment
    ) where

import Engine (Action, JobID, Result(..), action)

-- Some types we use for defining actions
-- Most of these can only be the result of a job, one of them (Template)
-- could also be retrieved from the model
data Disk = DiskJobResult JobID
  deriving (Show)
data Target = TargetJobResult JobID
  deriving (Show)
data Template = TemplateJobResult JobID
              | TemplateFromModel Int
  deriving (Show)
data Machine = MachineJobResult JobID
  deriving (Show)

-- We need a way to wrap (the identifier of) a job result in the
-- corresponding type
instance Result Disk where
    fromJobResult = DiskJobResult
instance Result Target where
    fromJobResult = TargetJobResult
instance Result Template where
    fromJobResult = TemplateJobResult
instance Result Machine where
    fromJobResult = MachineJobResult

-- Finally, our actions
-- The given type defines the name of the action, and 2 types: the input
-- type of an action, and the type of what it returns.
-- '()' signifies "void"

-- createDiskClone takes a Template and results in a Disk
createDiskClone :: Action "createDiskClone" Template Disk
createDiskClone = action
-- Similar, exposeISCSI take a Disk and results in a Target being created
exposeISCSI :: Action "exposeISCSI" Disk Target
exposeISCSI = action
-- createMachine takes a Template and a Target, and results in a new
-- Machine to be created
createMachine :: Action "createMachine" (Template, Target) Machine
createMachine = action
-- startMachine takes a Machine and has no result
startMachine :: Action "startMachine" Machine ()
startMachine = action

-- validateEnvironment is a demo action which takes no argument, and has no
-- result
validateEnvironment :: Action "validateEnvironment" () ()
validateEnvironment = action
