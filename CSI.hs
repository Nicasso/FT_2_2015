-- Week 1: CSI.hs
-- FT_02: Nico de Groot, Robin Kulhan, Andre Tavares

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Declarations of all the things the boys said
matthewSays, peterSays, jackSays, arnoldSays, carlSays :: Boy -> Bool
matthewSays = \x -> not (x == Matthew) && not (x == Carl)
peterSays = \x -> x == Matthew || x == Jack
jackSays = \x -> not (matthewSays x) && not (peterSays x)
arnoldSays = \x -> matthewSays x /= peterSays x
carlSays = \x -> not (arnoldSays x)

-- Return a list of accusers for each boy
accusers :: Boy -> [Boy]
accusers x | x == Matthew = [Peter, Jack, Arnold]
           | x == Peter = [Jack, Arnold]
           | x == Jack = [Peter]
           | x == Arnold = [Carl]
           | x == Carl = []

-- Checks which boys(s) are guilty
guilty :: [Boy]
guilty = [ (x) | x <- [Matthew, Peter, Jack, Arnold, Carl], (((matthewSays (x) == peterSays (x)) == jackSays (x)) == arnoldSays (x)) == carlSays (x) ]

-- Checks for each of the boys if they are honest
honest :: [Boy]
honest = [ (x) | x <- [Matthew, Peter, Jack, Arnold, Carl], checkHonesty x]

-- Checks if their declariations are true or false
checkHonesty :: Boy -> Bool
checkHonesty x | x == Matthew = not (elem Carl guilty) && not (elem Matthew guilty)
               | x == Peter = elem Matthew guilty || elem Jack guilty
               | x == Jack = not (checkHonesty Matthew) && not (checkHonesty Peter)
               | x == Arnold = checkHonesty Matthew /= checkHonesty Peter
               | x == Carl = not (checkHonesty Arnold)