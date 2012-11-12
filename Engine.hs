{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module Engine (
      evalWorkflow
    , log
    , runAction
    , runJob
    , Action
    , JobID
    , Result(..)
    , action
    , Job, defineJob
    , Workflow, defineWorkflow
    ) where

import Prelude hiding (log)

import Control.Applicative
import Control.Monad.State
import GHC.TypeLits

{- Action: Something executed in the real world, returning something
 - Job: Execution of multiple actions, returning something
 - Workflow: A Job which returns nothing
 -}

data Action (n :: Symbol) a b

class Result a where
    fromJobResult :: JobID -> a

instance Result () where
    fromJobResult _ = undefined

action :: SingI n => Action n a b
action = undefined

actionName :: SingI n => Action n a b -> String
actionName a = withSing (helper a)
  where
    helper :: Action n a b -> Sing n -> String
    helper _ n = fromSing n


type JobID = [Int]

data Command where
    CRunAction :: (SingI n, Show a) => JobID -> Action n a b -> a -> Command
    CLog :: String -> Command

instance Show Command where
    show (CRunAction i a b) = "CRunAction " ++ show i ++ " " ++ show (actionName a) ++ " (" ++ show b ++ ")"
    show (CLog s) = "CLog " ++ show s

type ActionTrackerState = ([Command], [Int])
newtype ActionTracker a = ActionTracker (State ActionTrackerState a)
  deriving (Functor, Applicative, Monad, MonadState ActionTrackerState)

newtype Job (n :: Symbol) a b = Job (a -> ActionTracker b)
type Workflow n a = Job n a ()

defineJob :: (a -> ActionTracker b) -> Job n a b
defineJob = Job
defineWorkflow :: (a -> ActionTracker ()) -> Workflow n a
defineWorkflow = Job

runAction :: (SingI n, Show a, Result b) => Action n a b -> a -> ActionTracker b
runAction act a = do
    (acts, is@(h:t)) <- get
    put (CRunAction is act a : acts, (h + 1) : t)
    return $ fromJobResult is

runJob :: SingI n => Job n a b -> a -> ActionTracker b
runJob job@(Job j) a = do
    log $ "Launching subjob " ++ jobName job

    (acts, is@(h:t)) <- get

    let (ActionTracker m) = j a
        (r, (newActs, _)) = runState m ([], 0 : is)

    put (newActs ++ acts, (h + 1) : t)

    log $ "Subjob " ++ jobName job ++ " completed"

    return r

log :: String -> ActionTracker ()
log m = modify (\(acts, gen) -> (CLog m : acts, gen))

jobName :: SingI n => Job n a b -> String
jobName a = withSing (helper a)
  where
    helper :: Job n a b -> Sing n -> String
    helper _ n = fromSing n

evalJob :: Job n a b -> a -> [Command]
evalJob (Job j) a = let (ActionTracker m) = j a in reverse $ fst $ snd $ runState m ([], [0])

evalWorkflow :: Workflow n a -> a -> [Command]
evalWorkflow = evalJob
