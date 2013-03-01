{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module VPA where

import Automata

{-| 
 - Datentyp "Visibly Pushdown Automata".
 - Die Sprachmange der Visibily Pushdown Autmata (VPA) sind eine echte Teilklasse der Kontextfreien Sprachen.
 - Sie haben gegen�ber den Kellerautomaten den Vorteil das sie 
 - unter allen typischen Boolschen Operationen Abgeschlossen.
 - Die VPAs arbeiten auf  einen "getaggten" Alphabet. Das Alphabet wird in 3 Klassen eingeteilt:
 -   1. Call Symbole
 -   2. Return Symbole
 -   3. Internal Symbole
 - F�r jede dieser Eingabesymbole gibt es eigene Zustands�berg�nge.
 -}
data (Eq stack) => VPA stack state alph = 
     VPA { -- | Test ob das Symbol aus dem Alphabet ein Return-Symbol ist.
           vpaIsReturn :: alph -> Bool, 
           
           -- | Test ob das Symbol aus dem Alphabet ein Call-Symbol ist.
           vpaIsCall :: alph -> Bool, 
           
           -- | Test ob das Symbol aus dem Alphabet ein Internal-Symbol ist.
           vpaIsInit :: alph -> Bool, 
           
           -- | Die Zust�nde des Automaten 
           vpaStates :: [state], 

           -- | Der Start-Zustand des Automaten
           vpaInitial :: state, 

           -- | Die akzeptierenden Zust�nde des Automaten
           vpaFinal :: [state], 

           -- | Der Fehlerzustand.
           vpaFail :: state, 

           -- | Das untere Kellersymbol
           vpaBot :: stack, 

           -- | �bergangsfunktion f�r Call-Symbole
           vpaCall :: state -> alph -> (state, stack), 

           -- | �bergangsfunktion f�r Return-Symbole
           vpaReturn :: state -> stack -> alph -> state, 

           -- | �bergangsfunktion f�r Internal-Symbole
           vpaInt :: state -> alph -> state 
         }


instance (Eq stack) => Automata (VPA stack) where
    states         = vpaStates
    initial        = (:[]) . vpaInitial
    final          = vpaFinal
    failure        = vpaFail
    stepE vpa q w  = fst $ flip3to1 vpaDeltaE [] vpa q w
    step _ _ _     = error "Not implemented"
    run vpa q w    = map fst $ flip3to1 vpaRun [] vpa q w

--instance (Automata (VPA stack)) => Transducer (VPA stack) where

flip3to1 :: (a -> b -> x -> e) -> x -> a -> b -> e
flip3to1 f x a b  = f a b x

-- Zustands�berf�hrung
vpaDelta :: (Eq s) => VPA s q a -> q -> [s] -> a -> (q,[s])
vpaDelta vpa st [] i     = vpaDelta vpa st [vpaBot vpa] i
vpaDelta vpa st (x : sx) i
  | vpaIsInit vpa i = (vpaInt vpa st i, x : sx)
  | vpaIsCall vpa i = if x == vpaBot vpa && nstack == vpaBot vpa 
                         then (nstate, x : sx)
                         else (nstate, nstack : x : sx)
  | vpaIsReturn vpa i = if x == vpaBot vpa 
                         then (nrstte, x : sx) 
                         else (nrstte, sx)
  | otherwise = (vpaFail vpa, x : sx)
  where nstate = fst $ vpaCall vpa st i
        nstack = snd $ vpaCall vpa st i
        nrstte = vpaReturn vpa st x i

vpaRun :: (Eq s) => VPA s q a -> q -> [s] -> [a] -> [(q,[s])]
vpaRun vpa q st (x:xs) = (q,st) : vpaRun vpa nstate nstack xs
    where nstate = fst $ vpaDelta vpa q st x
          nstack = snd $ vpaDelta vpa q st x
vpaRun _ _ [] _      = []

-- Erweiterte Zustand�berf�hrung
vpaDeltaE :: (Eq s) => VPA s q a -> q -> [s] -> [a] -> (q,[s])
vpaDeltaE _ q st []     = (q,st)
vpaDeltaE vpa q st (x:xs) = vpaDeltaE vpa nstate nstack xs
    where nstate = fst $ vpaDelta vpa q st x
          nstack = snd $ vpaDelta vpa q st x


-- Simple Example
--
--    +---+ a:0 +---+  b  +---+ c,0 #---#
-- -->+ 0 +---->+ 1 +---->+ 2 +----># 3 #
--    +---+     +---+     +---+     #---#
--              |   ^     |   ^
--              |---|     |---|
--               a:1       c,1

vpaABC :: VPA Int Int Char
vpaABC = VPA abcIsRet abcIsCall abcIsInt abcStates abcIntial abcFinal abcFail abcBot abcCall abcRet abcInt

abcIsRet :: Char -> Bool
abcIsRet 'c' = True
abcIsRet _   = False

abcIsCall :: Char -> Bool
abcIsCall 'a' = True
abcIsCall _   = False

abcIsInt :: Char -> Bool
abcIsInt 'a' = False
abcIsInt 'c' = False
abcIsInt _   = True

abcStates :: [Int]
abcStates = [0,1,2,3]

abcIntial :: Int
abcIntial = 0

abcFinal :: [Int]
abcFinal = [3]

abcBot :: Int
abcBot = 0

abcFail :: Int
abcFail = -1

abcCall :: forall a. (Eq a, Num a) => a -> Char -> (Int, Int)
abcCall 0 'a' = (1, abcBot)
abcCall 1 'a' = (1, 1)
abcCall _ _   = (abcFail, abcBot)

abcRet :: forall a a1.
                     (Eq a1, Eq a, Num a1, Num a) =>
                     a -> a1 -> Char -> Int
abcRet 2 1 'c' = 2
abcRet 2 0 'c' = 3
abcRet _ _ _        = abcFail

abcInt :: forall a. (Eq a, Num a) => a -> Char -> Int
abcInt 1 'b' = 2
abcInt _ _   = abcFail

