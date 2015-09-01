data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

matthewSays, peterSays, jackSays, arnoldSays, carlSays :: Boy -> Bool
matthewSays = \x -> not (x == Matthew) && not (x == Carl)
peterSays = \x -> x == Matthew || x == Jack
jackSays = \x -> not (matthewSays x) && not (peterSays x)
arnoldSays = \x -> matthewSays x /= peterSays x
carlSays = \x -> not (arnoldSays x)

matthewAccusers, peterAccusers, jackAccusers, arnoldAccusers, carlAccusers :: Boy -> [Boy]
matthewAccusers x = [Peter, Jack]
peterAccusers x = [Jack]
jackAccusers x = [Peter]
arnoldAccusers x = [Carl]
carlAccusers x = []

accusers :: Boy -> [Boy]
accusers x | x == Matthew = [Peter, Jack, Arnold]
           | x == Peter = [Jack, Arnold]
           | x == Jack = [Peter]
           | x == Arnold = [Carl]
           | x == Carl = []

guilty :: [Boy]
guilty = [ (x) | x <- [Matthew, Peter, Jack, Arnold, Carl], (((matthewSays (x) == peterSays (x)) == jackSays (x)) == arnoldSays (x)) == carlSays (x) ]

honest :: [Boy]
honest = [ (x) | x <- [Matthew, Peter, Jack, Arnold, Carl], checkHonesty x]

checkHonesty :: Boy -> Bool
checkHonesty x | x == Matthew = not (elem Carl guilty) && not (elem Matthew guilty)
               | x == Peter = elem Matthew guilty || elem Jack guilty
               | x == Jack = not checkMatthew && not checkPeter
               | x == Arnold = checkMatthew /= checkPeter
               | x == Carl = not checkArnold

-- Honest: Peter, Matthew, Carl
-- False: Jack, Arnold

data Creature = Lady | Tiger deriving (Eq,Show)

sign1, sign2 :: (Creature,Creature) -> Bool
sign1 (x,y) = x == Lady && y == Tiger
sign2 (x,y) = x /= y

solution1 :: [(Creature,Creature)]
solution1 = [ (x,y) | x <- [Lady,Tiger], y <- [Lady,Tiger], sign1 (x,y) /= sign2 (x,y) ]