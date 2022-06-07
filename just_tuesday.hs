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

-- | Default entry point.
main :: IO ()
main = runWith ["buy milk", "grade hw"]

runWith :: [Task] -> IO ()
runWith tasks = do
    --putStrLn "Hello, world"
    input <- getLine
    case input of
        "/exit" -> do
            putStrLn "Bye!"
        "/show" -> do
            printAllTasks tasks
            runWith tasks
        "/add" -> do
            taskStr <- getLine
            runWith (taskStr:tasks)
        "/done" -> do
            indexStr <- getLine
            case readMaybe indexStr of
                Nothing -> do
                    putStrLn "ERROR: invalid index"
                    runWith tasks
                Just i -> do
                    let (leftTasks, mbRemovedTask) = remove i tasks
                    case mbRemovedTask of
                        Nothing -> do
                            putStrLn "ERROR: no task with such index"
                        Just removedTask -> do
                                putStrLn ("Removed task " ++ removedTask)
                    runWith leftTasks
        _ -> do
            putStrLn ("You said: " ++ input)
            runWith tasks
