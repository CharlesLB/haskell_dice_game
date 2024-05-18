module Types.Move where

type Index = Int

data Move
  = UpdateMove {updateIndex :: Index, newValue :: Int}
  | RemoveMove {removeIndex :: Index}
  deriving (Show)
