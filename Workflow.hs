{-# LANGUAGE DataKinds #-}

module Workflow (
      deployMachine
    , deployAndStart
    ) where

import Prelude hiding (log)
import Engine
import Actions

-- The deployMachine job takes a template, and will finally result in
-- a machine being created
deployMachine :: Job "deployMachine" Template Machine
deployMachine = defineJob $ \template -> do
    disk <- tryCatch
            (do
                disk <- runAction createDiskClone template
                log "Disk cloned"
                return disk)
            (do
                log "Disk clone failed"
                stop)

    log "Created disk"
    target <- runAction exposeISCSI disk
    machine <- runAction createMachine (template, target)
    log "Created machine"
    return machine

-- deployAndStart is a Workflow. This is similar to a Job, but it doesn't
-- have any result. This one takes a Template as its argument.
-- The type declaration is optional
deployAndStart :: Workflow "deployAndStart" Template
deployAndStart = defineWorkflow $ \template -> do
    runAction validateEnvironment ()
    machine <- runJob deployMachine template
    runAction startMachine machine
    log "Machine created and launched"
