{-# LANGUAGE ScopedTypeVariables #-}

module GUIVPT where

import Data.Maybe
import VPT
import Automata
import GUIAutomata

{- TODO:
 - removeStates: Was ist wenn der Entfernte zustand der Selektierte ist? Oder Gar der Startzustand
-}

--isInState :: VVPT -> (Double,Double) -> Maybe Int
--isInState vvpt p = foldl fkt Nothing (pos vvpt)
--   where fkt Nothing (i,q) = if incircle p q drawRadius then Just i else Nothing
--         fkt (Just a) _    = Just a
--    
     
-- | Diese Funktion gibt den Maximal Idx zurück
maxIndex :: GUIVPT -> Int
maxIndex vpt = maximum (map fst (gvPos vpt)++[0])

--         
---- | Diese Funktion fügt eine Kante ein
--vptInsertEdge :: VVPT -- ^ Die Autenrepräsentation
--            -> Int    -- ^ Startzustand
--            -> Int    -- ^ Zielzustand
--            -> Char   -- ^ Eingabesymbol
--            -> String -- ^ Ausgabesymbol
--            -> VVPT   -- ^ Das neue Automatenmodell
--vptInsertEdge vvpt i j e a = VVPT (alphabet vvpt) (pos vvpt) ((i, j, e, a) : links vvpt) (ziele vvpt) (starte vvpt) 
--
---- | Diese Funktion fügt einen Zustand ein.
--vptAddState :: VVPT -> (Double,Double) -> (VVPT,Int)
--vptAddState vvpt p = (VVPT (alphabet vvpt) ((m + 1, p) : pos vvpt) (links vvpt) (ziele vvpt) (starte vvpt),m+1)
--        where m = vptMaxIndex vvpt 
--
---- | Diese Funktion gibt die Position eines State zurück
--vptPos :: VVPT -> Int -> Maybe (Double,Double)
--vptPos vvpt i = foldl myfkt Nothing (pos vvpt)
--    where myfkt Nothing (j,p) = if i == j then Just p else Nothing
--          myfkt (Just p) _    = Just p
--          myftk _  _          = Nothing
--          
---- | Diese Funktion ändert die Position eines States
--vptMove :: VVPT -> Int -> (Double,Double) -> VVPT
--vptMove vvpt i p = VVPT (alphabet vvpt) (map mf $ pos vvpt) (links vvpt) (ziele vvpt) (starte vvpt)
--    where mf :: (Int, (Double,Double)) -> (Int, (Double,Double))
--          mf (j,q) = if i == j then (j,p) else (j,q)

data GUIVPT =
  GUIVPT { gvAlpha :: (String, String, String), 
           gvPos   :: [(Int, (Double,Double))], 
           gvLinks :: [(Int,Int,Char,Maybe Char,Maybe Char, Maybe String)],
           gvStart :: Maybe Int,
           gvFinal :: [Int]
         }
         
emptyGuiVpt :: GUIVPT
emptyGuiVpt = GUIVPT ([],[],[]) [] [] Nothing []
         
instance GUIAutomata GUIVPT where
   automata vpt = VPT (flip elem $ isReturn) (flip elem $ isCall) (flip elem $ isReturn) (vStates::[Int]) (gvStart vpt) (gvFinal vpt) (-1) '#' ('\0') call retr init Nothing ['#']
           where (isReturn,isCall,isInit) = gvAlpha vpt
                 vStates                  = getStates vpt
                 mapMe3 p w = mapMaybe (\z@(i,j,k,_,_,_) -> if i == p && k == w then Nothing else Just z)
                 get p w = mapMe3 p w $ gvLinks vpt
                 call state alph = if null $ get state alph then (-1,'\0',"\0") else case head $ get state alph of { (z1,z2,i,r,c,o) -> (z2,fromJust c,fromJust o) } -- Todo Stack
                 retr state alph stack  = if null $ get state alph then (-1,"") else case head $ get state alph of { (z1,z2,i,r,c,o) -> (z2,fromJust o) } -- Todo Stack
                 init state alph  = if null $ get state alph then (-1,"") else case head $ get state alph of { (z1,z2,i,r,c,o) -> (z2,fromJust o) } -- Todo Stack
                 

   isTransducer _ = True
   
   getStatesOnPos vpt u r = mapMaybe (\(p,v) -> if incircle u v r then Just p else Nothing) (gvPos vpt)
   
   setStatePos vpt q p =  GUIVPT (gvAlpha vpt) (map mf $ gvPos vpt)  (gvLinks vpt) (gvStart vpt) (gvFinal vpt)
                  where mf :: (Int, (Double,Double)) -> (Int, (Double,Double))
                        mf (j,z) = if q == j then (j,p) else (j,z)
   
   getStatePos vpt i = foldl myfkt Nothing (gvPos vpt)
          where myfkt Nothing (j,p) = if i == j then Just p else Nothing
                myfkt (Just p) _    = Just p
                myftk _  _          = Nothing
                  
   setStateStart vpt q True = if q `elem` getStates vpt then GUIVPT (gvAlpha vpt) (gvPos vpt) (gvLinks vpt) (Just q) (gvFinal vpt) else vpt
   setStateStart vpt q False = if isNothing $ gvStart vpt then vpt else if q == (fromJust . gvStart $ vpt) then GUIVPT (gvAlpha vpt) (gvPos vpt) (gvLinks vpt) (Nothing) (gvFinal vpt) else vpt
   
   getStateStart vpt = gvStart vpt

   setStateFinal gvpt q False = if q `elem` gvFinal gvpt then GUIVPT (gvAlpha gvpt) (gvPos gvpt) (gvLinks gvpt) (gvStart gvpt) (filter ((==)q) $ gvFinal gvpt) else gvpt
   setStateFinal gvpt q True = if q `elem` getStates gvpt then GUIVPT (gvAlpha gvpt) (gvPos gvpt) (gvLinks gvpt) (Just q) (gvFinal gvpt) else gvpt

   getStateFinal gvpt = gvFinal gvpt
   
   addState gvpt q (x,y) = if q `elem` getStates gvpt then gvpt else GUIVPT (gvAlpha gvpt) ((q,(x,y)):gvPos gvpt) (gvLinks gvpt) (gvStart gvpt) (gvFinal gvpt)

   removeState gvpt q = if q `elem` getStates gvpt then  GUIVPT (gvAlpha gvpt)  (mapMe $ gvPos gvpt) (mapMe2 $ gvLinks gvpt) (st) (mapMeOne $ gvFinal gvpt) else gvpt
                  where mapMe = mapMaybe (\(p,z) -> if p == q then Nothing else Just (p,z))
                        mapMeOne = mapMaybe (\p -> if p == q then Nothing else Just p)
                        mapMe2 = mapMaybe (\z@(s1,s2,i,w,c,o) -> if s1 == q || s2 == q then Nothing else Just z)
                        st = if isJust $ gvStart gvpt then if (fromJust (gvStart gvpt)) == q then Nothing else (gvStart gvpt) else Nothing
                        
   removeEdge gvpt q p c = GUIVPT (gvAlpha gvpt)  (gvPos gvpt) (mapMe3 $ gvLinks gvpt) (gvStart gvpt) (gvFinal gvpt)
                  where mapMe3 = mapMaybe (\z@(s1,s2,i,w,call,o) -> if s1 == q && s2 == p && i == c then Nothing else Just z)
   
   addEdge gvpt q p i r w o = if cont then gvpt else GUIVPT (gvAlpha gvpt)  (gvPos gvpt) ((q,p,i,r,w,o) : gvLinks gvpt) (gvStart gvpt) (gvFinal gvpt)
                 where cont = foldl (\t (vq,vp,vw,_,_,_) -> t || (vq == q && vp == p && vw == i) ) False (gvLinks gvpt)
   
   getStates gvpt = map (\(q,_) -> q) $ gvPos gvpt
               
   getEdges gvpt = gvLinks gvpt
