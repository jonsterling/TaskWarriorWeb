module Main where
import TaskWarriorWeb

import Yesod

main :: IO ()
main = warpDebug 3000 TaskWarrior