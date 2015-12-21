module Tunagui.Primitive.Button where

data Button

newButton :: IO Button
newButton = do
  putStrLn "Add code making new Button here."
  -- * Make button considering some settings from arguments
  -- * Allocate video resource
  return Button

freeButton :: Button -> IO ()
freeButton button = do
  putStrLn "Add code freeing Button here."
  return ()
