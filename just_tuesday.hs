--module Main where

import Text.Read (readMaybe)

type Task = String

printAllTasks :: [Task] -> IO ()
printAllTasks [] = putStrLn "No tasks yet."
printAllTasks tasks = printAllTasks' tasks
    where
        printAllTasks' :: [Task] -> IO ()
        printAllTasks' [] = putStrLn ""
        printAllTasks' (task:tasks) = do
        putStrLn task
        printAllTasks' tasks

remove :: Integer -> [Task] -> ([Task], Maybe Task)
remove _ [] = ([], Nothing)
remove 0 (x:xs) = (xs, Just x)
remove n (x:xs) = (x:left, removed)
        where
                (left, removed) = remove (n-1) xs

data Command
    = Exit
    | ShowTasks
    | RemoveTask Integer
    | AddTask Task

type State = [Task]

handleCommand :: Command -> State -> IO (Maybe State)
handleCommand Exit _ = return Nothing
handleCommand ShowTasks tasks = do
    printAllTasks tasks
    return $ Just tasks
handleCommand (AddTask newTask) tasks = do
    return $ Just (newTask : tasks)
handleCommand (RemoveTask taskIndex) tasks
    = return $ Just $ fst $ remove taskIndex tasks

getCommand :: IO (Maybe Command)
getCommand = do
    input <- getLine
    case input of
        "/exit" -> return $ Just Exit
        "/show" -> return $ Just ShowTasks
        "/done" -> do
            indexStr <- getLine
            case readMaybe indexStr of
                Nothing -> do
                    putStrLn "ERROR: invalid index"
                    return Nothing
                Just i -> return $ Just $ RemoveTask i
        "/add" -> do
            taskName <- getLine
            return $ Just $ AddTask taskName
        _ -> return Nothing

-- | Default entry point.
main :: IO ()
main = runWith ["buy milk", "grade hw"]

runWith :: [Task] -> IO ()
runWith tasks = do
    putStrLn "command> "

    command <- getCommand

    newTasks <-
            case command of
                Nothing -> do
                    putStrLn "ERROR: unrecognized command"
                    return $ Just tasks
                Just command' -> do
                    handleCommand command' tasks

    case newTasks of
        Nothing -> putStrLn "Bye!"
        Just newTasks' -> runWith newTasks'
