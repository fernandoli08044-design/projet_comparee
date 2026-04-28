import System.Random (StdGen, randomR,getStdGen)

somme :: [Integer] -> Integer 
somme [] = 0
somme (h : t) = h + (somme t)

ilExiste :: (a -> Bool) -> [a] -> Bool
ilExiste _ [] = False
ilExiste f (h:t) = (f h) || ilExiste f t 

jsqZer :: Integer -> [Integer]
jsqZer 0 = [0]
jsqZer a = a : (jsqZer (a-1))

depZer :: Integer -> [Integer]
depZer 0 = [0]
depZer a = (depZer (a-1)) <> [a] 

zeros :: Integer -> [Integer]
zeros 0 = []
zeros n = [0] ++ (zeros (n-1))




-- des types du jeu 

data Ecran = Ecran {
                    ecrHt :: Integer,
                    ecrLg :: Integer
                    }

data Coord = C{
                cx :: Integer,
                cy :: Integer
              } deriving Eq

data Obstacle = Caillou Coord

data Joueuse = Joueuse {
                         jCoord :: Coord,
                         jPv :: Integer
                       } 

data Statut = Gagne | Perdu | EnCours
              deriving (Show,Eq)

data Envi = Envi {
                  envEcr :: Ecran,
                  envJou :: Joueuse,
                  envObs :: [Obstacle],
                  envst :: Statut,
                  envGen :: StdGen
                 }

data Case= OBS | JOU | VIDE
            deriving Eq

instance Show Case where
    show OBS = "0"
    show JOU = "^"
    show VIDE = " "

-- Environnement du jeu

toucheObs :: Coord -> Obstacle -> Bool
toucheObs (C x y)(Caillou(C x' y')) = (x==x' && y==y')

contenu :: Coord -> Envi -> Case
contenu co (Envi ecran jou obs st _) | jCoord jou == co = JOU
                                   | ilExiste (\o -> toucheObs co o) obs = OBS
                                   | otherwise = VIDE

instance Show Envi where
   show env | envst env /= Perdu =   foldr (\y acc -> foldr (ligne env y)("\n"<> acc)(depZer (ecrLg(envEcr env))))
                                     "\n"
                                     (jsqZer(ecrHt(envEcr env)))
                                     <> "PV: "<> show (jPv (envJou env)) <> "\n"     --syntaxe inversée ?
            |otherwise = "Perdu !!! "
             where ligne env y x acc = show (contenu (C x y) env) <> acc 

-- Monade d'Etat

data Etat s a = Etat (s -> (s,a))

instance Functor (Etat s) where
    -- fmap :: (a -> b) -> Etat s a -> Etat s b
    fmap f (Etat p1) = Etat (\x ->
        let (x', y) = p1 x
        in (x', f y)
      )

instance Applicative (Etat s) where
    -- pure :: a -> Etat s a
    pure y = Etat (\x -> (x, y))

    -- (<*>) :: Etat s (a -> b) -> Etat s a -> Etat s b
    (<*>) (Etat pf) (Etat py) = Etat (\x ->
        let (x', f)  = pf x
            (x'', y) = py x'
        in (x'', f y)
      )

instance Monad (Etat s) where
    -- (>>=) :: Etat s a -> (a -> Etat s b) -> Etat s b
    (>>=) (Etat p1) f = Etat (\x ->
        let (x', y) = p1 x in 
        let (Etat p2) = f y
        in p2 x'
      )


type EtatJeu = Etat Envi

get :: Etat s s
get = Etat (\s -> (s, s))

put :: s -> Etat s ()
put s = Etat (\_ -> (s, ()))

data Direction = H | B | G | D | N 
    deriving Eq


-- Joueuse

depJ :: Char -> EtatJeu ()
depJ 'z' = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen )-> ((Envi ecr (Joueuse (C x (y+1)) pv )obs st gen ),()))

depJ 's' = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen )-> ((Envi ecr (Joueuse (C x (y-1)) pv )obs st gen ),()))
depJ 'q' = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen )-> ((Envi ecr (Joueuse (C (x-1) y) pv )obs st gen ),()))
depJ 'd' = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen )-> ((Envi ecr (Joueuse (C (x+1) y) pv )obs st gen ),()))
depJ _ = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen )-> ((Envi ecr (Joueuse (C x y) pv )obs st gen),()))

checkPerdu :: EtatJeu()
checkPerdu = Etat(\(Envi ecr (Joueuse c pv ) obs st gen) -> (if pv <=0 then (Envi ecr (Joueuse c pv) obs Perdu gen)
                                                                    else (Envi ecr (Joueuse c pv) obs st gen)
                                                        ,()))

--  perte de PV
pertePVJo :: Joueuse ->Joueuse 
pertePVJo ( Joueuse co pv ) = Joueuse co (pv-1)

obsPVJo :: Envi -> Integer
obsPVJo (Envi _ (Joueuse _ pv) _ _ _ ) = pv


obsPVEnv :: EtatJeu Integer
obsPVEnv = Etat(\(Envi ecr (Joueuse c pv) obs st gen) -> ((Envi ecr (Joueuse c pv) obs st gen),pv))

obsSt :: EtatJeu Statut
obsSt = Etat(\(Envi ecr jo obs st gen) -> ((Envi ecr jo obs st gen ),st))

affiche ::EtatJeu String 
affiche = Etat (\env -> (env,show env))

pertePVEnv :: Envi -> Envi
pertePVEnv(Envi ecr jo obs st gen) | obsPVJo (Envi ecr jo obs st gen ) >1 = Envi ecr (pertePVJo jo) obs st gen
                               | otherwise = Envi ecr (pertePVJo jo) obs Perdu gen
pertePV :: EtatJeu()
pertePV = Etat(\env -> (pertePVEnv env,()))

script1 :: EtatJeu Statut
script1 = do 
            pertePV
            pertePV
            pertePV
            obsSt

script2 :: EtatJeu Integer
script2 = do 
            pertePV
            pertePV
            obsPVEnv

script3 :: EtatJeu[(Integer,Statut)]
script3 = do 
            pv1 <- script2 
            st1 <- obsSt
            pv2 <- script2 
            st2 <- obsSt
            pv3 <- script2 
            st3 <- obsSt
            return [(pv1,st1) , (pv2,st2) , (pv3,st3)]                                         --return permet d'avoir à la fin un type de EtatJeu() au lieu de juste [(Integer,Statut)]

script4 :: EtatJeu(String)
script4 = do 
            scroll 
            scroll 
            scroll 
            scroll 
            affiche


applique :: Envi -> EtatJeu a -> a 
applique env (Etat p) = let (_ ,res) = p env in res



--scrolling 
descendUn :: Obstacle -> Obstacle 
descendUn  (Caillou(C x y )) = Caillou (C x (y-1))

scrollEnv :: Envi -> Envi
scrollEnv (Envi ecr (Joueuse c pv ) obs st gen ) | (ilExiste (toucheObs c ) obs )=Envi ecr (Joueuse c (pv-1)) (fmap descendUn obs) st gen
                                             | otherwise = Envi ecr (Joueuse c pv) (fmap descendUn obs) st gen

cleanObs :: Envi -> Envi
cleanObs env =
    let h = ecrHt (envEcr env)
        obs' = filter (\(Caillou (C _ y)) -> y >= 0 && y <= h) (envObs env)
    in env { envObs = obs' }

scroll :: EtatJeu ()
scroll = Etat(\env ->
    let env' = scrollEnv env
    in (env',()))


tour :: Char -> EtatJeu String
tour c = do 
            depJ c
            scroll
            checkPerdu
            spawnObs 3
            affiche


-- partie aléatoire 

genList :: Integer -> StdGen -> Integer -> ([Integer], StdGen)              
genList 0 gen _ = ([], gen)
genList n gen maxX =
    let (v, gen1) = randomR (0, maxX) gen
        (rest, gen2) = genList (n-1) gen1 maxX
    in (v : rest, gen2)

randomList :: Integer -> EtatJeu [Integer]
randomList n = Etat ( \env ->
    let gen = envGen env
        (vals, gen') = genList n gen (ecrLg (envEcr env))

    in (env { envGen = gen' }, vals))

spawnObs :: Integer -> EtatJeu ()
spawnObs n = Etat (\env ->
    let (xs, gen') = genList n (envGen env) (ecrLg (envEcr env))
        topY = ecrHt (envEcr env)
        newObs = map (\x -> Caillou (C x topY)) xs
    in (env { envObs = newObs ++ envObs env, envGen = gen' }, ()))

-- getChar :: IO getChar
-- putStrLn :: String-> IO()

-- ***   PARTIE IMPURE   ***

faitTour :: Char-> Envi -> IO Envi
faitTour c env = let Etat p = (tour c) in 
                 let (env',str) = p env in 
                 do 
                    putStrLn str
                    return env'


boucle :: Envi ->IO()
boucle env = do 
                c <- getChar
                env' <- faitTour c env
                boucle env'

main :: IO()
main = do
    gen <- getStdGen   -- 从 IO 里取出 StdGen
    let env0 = Envi
            (Ecran 30 60)
            (Joueuse (C 15 2) 3)
            []
            EnCours
            gen
    putStrLn (applique env0 affiche)
    boucle env0
