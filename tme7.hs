import System.Random (StdGen, randomR, getStdGen)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- ==========================================
-- FONCTIONS UTILITAIRES DE BASE
-- ==========================================

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

-- ==========================================
-- TYPES DU JEU (Modèle de données)
-- ==========================================

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
                  envGen :: StdGen,
                  envScore :: Integer  -- NOUVEAU: Le score du joueur
                 }

data Case= OBS | JOU | VIDE
            deriving Eq

instance Show Case where
    show OBS = "0"
    show JOU = "^"
    show VIDE = " "

toucheObs :: Coord -> Obstacle -> Bool
toucheObs (C x y)(Caillou(C x' y')) = (x==x' && y==y')

contenu :: Coord -> Envi -> Case
contenu co (Envi ecran jou obs st _ _) | jCoord jou == co = JOU
                                       | ilExiste (\o -> toucheObs co o) obs = OBS
                                       | otherwise = VIDE

instance Show Envi where
   show env | envst env /= Perdu =   foldr (\y acc -> foldr (ligne env y)("\n"<> acc)(depZer (ecrLg(envEcr env))))
                                     "\n"
                                     (jsqZer(ecrHt(envEcr env)))
                                     <> "PV: "<> show (jPv (envJou env)) <> " | Score: " <> show (envScore env) <> "\n"     
            |otherwise = "Perdu !!! "
             where ligne env y x acc = show (contenu (C x y) env) <> acc 

-- ==========================================
-- MONADE D'ETAT
-- ==========================================

data Etat s a = Etat (s -> (s,a))

instance Functor (Etat s) where
    fmap f (Etat p1) = Etat (\x ->
        let (x', y) = p1 x
        in (x', f y)
      )

instance Applicative (Etat s) where
    pure y = Etat (\x -> (x, y))
    (<*>) (Etat pf) (Etat py) = Etat (\x ->
        let (x', f)  = pf x
            (x'', y) = py x'
        in (x'', f y)
      )

instance Monad (Etat s) where
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

-- ==========================================
-- ACTIONS PURES (Logique de jeu)
-- ==========================================

depJ :: Char -> EtatJeu ()
depJ 'z' = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen sc)-> ((Envi ecr (Joueuse (C x (y+1)) pv )obs st gen sc),()))
depJ 's' = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen sc)-> ((Envi ecr (Joueuse (C x (y-1)) pv )obs st gen sc),()))
depJ 'q' = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen sc)-> ((Envi ecr (Joueuse (C (x-1) y) pv )obs st gen sc),()))
depJ 'd' = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen sc)-> ((Envi ecr (Joueuse (C (x+1) y) pv )obs st gen sc),()))
depJ _   = Etat(\(Envi ecr (Joueuse (C x y) pv) obs st gen sc)-> ((Envi ecr (Joueuse (C x y) pv )obs st gen sc),()))

checkPerdu :: EtatJeu()
checkPerdu = Etat(\(Envi ecr (Joueuse c pv ) obs st gen sc) -> 
    (if pv <= 0 then (Envi ecr (Joueuse c pv) obs Perdu gen sc)
                else (Envi ecr (Joueuse c pv) obs st gen sc), ()))

pertePVJo :: Joueuse -> Joueuse 
pertePVJo ( Joueuse co pv ) = Joueuse co (pv-1)

obsPVJo :: Envi -> Integer
obsPVJo (Envi _ (Joueuse _ pv) _ _ _ _) = pv

obsPVEnv :: EtatJeu Integer
obsPVEnv = Etat(\(Envi ecr (Joueuse c pv) obs st gen sc) -> ((Envi ecr (Joueuse c pv) obs st gen sc),pv))

obsSt :: EtatJeu Statut
obsSt = Etat(\(Envi ecr jo obs st gen sc) -> ((Envi ecr jo obs st gen sc),st))

pertePVEnv :: Envi -> Envi
pertePVEnv(Envi ecr jo obs st gen sc) 
    | obsPVJo (Envi ecr jo obs st gen sc) > 1 = Envi ecr (pertePVJo jo) obs st gen sc
    | otherwise = Envi ecr (pertePVJo jo) obs Perdu gen sc

pertePV :: EtatJeu()
pertePV = Etat(\env -> (pertePVEnv env,()))

descendUn :: Obstacle -> Obstacle 
descendUn (Caillou(C x y)) = Caillou (C x (y-1))

scrollEnv :: Envi -> Envi
scrollEnv (Envi ecr (Joueuse c pv) obs st gen sc) 
    | ilExiste (toucheObs c) obs = Envi ecr (Joueuse c (pv-1)) (fmap descendUn obs) st gen sc
    | otherwise = Envi ecr (Joueuse c pv) (fmap descendUn obs) st gen sc

cleanObs :: Envi -> Envi
cleanObs env =
    let h = ecrHt (envEcr env)
        obs' = filter (\(Caillou (C _ y)) -> y >= 0 && y <= h) (envObs env)
    in env { envObs = obs' }

scroll :: EtatJeu ()
scroll = Etat(\env -> (scrollEnv env, ()))

genList :: Integer -> StdGen -> Integer -> ([Integer], StdGen)              
genList 0 gen _ = ([], gen)
genList n gen maxX =
    let (v, gen1) = randomR (0, maxX) gen
        (rest, gen2) = genList (n-1) gen1 maxX
    in (v : rest, gen2)

spawnObs :: Integer -> EtatJeu ()
spawnObs n = Etat (\env ->
    let (xs, gen') = genList n (envGen env) (ecrLg (envEcr env))
        topY = ecrHt (envEcr env)
        newObs = map (\x -> Caillou (C x topY)) xs
    in (env { envObs = newObs ++ envObs env, envGen = gen' }, ()))

gainScore :: EtatJeu ()
gainScore = Etat (\env -> (env { envScore = envScore env + 1 }, ()))


-- ==========================================
-- *** PARTIE GLOSS (Temps réel et Graphismes) ***
-- ==========================================

-- 1. Utilitaire pour déballer la monade Etat
execEtat :: Etat s a -> s -> s
execEtat (Etat p) env = let (env', _) = p env in env'

tailleCase :: Float
tailleCase = 20.0

-- 2. La Vue : dessiner l'environnement
dessiner :: Envi -> Picture
dessiner env 
    | envst env == Perdu = pictures [
        translate (-150) 0 $ scale 0.5 0.5 $ color red $ text "PERDU !!!",
        translate (-100) (-100) $ scale 0.2 0.2 $ color white $ text ("Score Final : " ++ show (envScore env))
      ]
    | otherwise = pictures [
        dessinerJoueur (envJou env), 
        dessinerObstacles (envObs env),
        dessinerUI env
      ]

dessinerJoueur :: Joueuse -> Picture
dessinerJoueur (Joueuse (C x y) _) = 
    translate (fromIntegral x * tailleCase - 400) (fromIntegral y * tailleCase - 300) $ 
    color cyan $ polygon [(-10, -10), (10, -10), (0, 15)]

dessinerObstacles :: [Obstacle] -> Picture
dessinerObstacles obs = pictures (map dessinerUn obs)
  where 
    dessinerUn (Caillou (C x y)) = 
        translate (fromIntegral x * tailleCase - 400) (fromIntegral y * tailleCase - 300) $ 
        color orange $ circleSolid 10

dessinerUI :: Envi -> Picture
dessinerUI env = pictures [
    translate (-350) (-250) $ scale 0.15 0.15 $ color white $ text ("PV: " ++ show (jPv (envJou env))),
    translate 150 (-250) $ scale 0.15 0.15 $ color yellow $ text ("SCORE: " ++ show (envScore env))
  ]

-- 3. Les Entrées : réagir au clavier
gererEntrees :: Event -> Envi -> Envi
gererEntrees (EventKey (Char k) Down _ _) env 
    | k `elem` ['z', 'q', 's', 'd'] = execEtat (depJ k) env
gererEntrees _ env = env

-- 4. Le Moteur : mettre à jour le jeu à chaque "frame"
mettreAJour :: Float -> Envi -> Envi
mettreAJour _ env 
    | envst env == Perdu = env
    | otherwise = execEtat tourContinu env
  where
    tourContinu = do
        scroll
        checkPerdu
        spawnObs 1 
        gainScore
        Etat (\e -> (cleanObs e, ()))

-- 5. Le lancement
main :: IO ()
main = do
    putStrLn "Lancement du Shoot'em Up graphique avec Score..."
    gen <- getStdGen   
    let env0 = Envi
            (Ecran 30 60)
            (Joueuse (C 15 2) 3)
            []
            EnCours
            gen
            0  -- Score de départ
            
    let fenetre = InWindow "PCOMP 2026 : Shoot'em Up" (800, 600) (100, 100)
    play fenetre black 5 env0 dessiner gererEntrees mettreAJour