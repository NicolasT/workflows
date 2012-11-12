module Demo (main) where

import Engine (evalWorkflow)
import Actions (Template(TemplateFromModel))
import Workflow (deployAndStart)

main :: IO ()
main = mapM_ print $ evalWorkflow deployAndStart (TemplateFromModel 321)
