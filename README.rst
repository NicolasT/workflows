Workflows
=========
This is some playground code to define a type-safe EDSL for defining workflows.

You'll need GHC 7.6 or higher to run the code (since it uses *DataKind*\s).

Demo
----
::

    $ runhaskell Demo.hs
    CRunAction [0] "validateEnvironment" (())
    CLog "Launching subjob deployMachine"
    CRunAction [0,1] "createDiskClone" (TemplateFromModel 321)
    CLog "Created disk"
    CRunAction [1,1] "exposeISCSI" (DiskJobResult [0,1])
    CRunAction [2,1] "createMachine" ((TemplateFromModel 321,TargetJobResult [1,1]))
    CLog "Created machine"
    CLog "Subjob deployMachine completed"
    CRunAction [2] "startMachine" (MachineJobResult [2,1])
    CLog "Machine created and launched"
