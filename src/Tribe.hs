module Tribe 
    ( Sex(..)
    , PuzzleAnswer(..)
    , sovlePuzzle
    )where 

import Control.Monad (guard) 

data Sex = Male | Female deriving (Eq, Show) 

data PuzzleAnswer = PuzzleAnswer { 
    parent1 :: Sex ,
    parent2 :: Sex ,
    child:: Sex ,
    child_desc:: Sex
}

instance Show PuzzleAnswer where 
    show pa = 
                "Parent1 is" ++ (show $ parent1 pa) ++ "\n" ++ 
                "Parent2 is" ++ (show $ parent2 pa) ++ "\n" ++ 
                "child is" ++ (show $ child pa) ++ "\n" ++ 
                "child said they were " ++ (show $ child_desc pa) ++ "\n"

childSatementIsValid :: Sex -> Sex -> Bool 
childSatementIsValid Male Female = False 
childSatementIsValid _ _ = True 

parent1StatmentIsValid :: Sex -> Sex -> Bool
parent1StatmentIsValid Male Female = False
parent1StatmentIsValid _ _ = True 

parent2StatmentIsValid :: Sex -> Sex -> Sex -> Bool
parent2StatmentIsValid Male Female Male = False
parent2StatmentIsValid Female _ Female = True 
parent2StatmentIsValid _ _ _ = False

sovlePuzzle :: (Sex -> Sex -> Bool) -> [PuzzleAnswer]
sovlePuzzle sexuality_pred = do 
    parent1 <- [Male,Female] 
    parent2 <- [Male,Female] 
    child <- [Male,Female]
    child_desc <- [Male,Female]
    guard $ sexuality_pred parent1 parent2 
    guard $ childSatementIsValid child child_desc
    guard $ parent1StatmentIsValid parent1 child_desc 
    guard $ parent2StatmentIsValid parent2 child child_desc 
    return $ PuzzleAnswer {
        parent1 = parent1,
        parent2 = parent2, 
        child = child ,
        child_desc = child_desc
    }
