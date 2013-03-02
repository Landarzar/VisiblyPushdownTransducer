module GUIAutomata where

import Automata
import VPT


incircle :: (Double,Double) -> (Double,Double) -> Double -> Bool
incircle (x,y) (xz,yz) r = (x-xz)*(x-xz) + (y - yz)*(y - yz) < r*r

-- | Diese Klasse stellt eine reihe von KonstruktionsFunktionen breit
class GUIAutomata mdl where
   automata       ::  mdl                  -- ^
                  -> VPT Char Char Int Char         -- ^ Der Automat

   isTransducer   :: mdl                   -- ^
                  -> Bool                  -- ^
   
   getStatesOnPos :: mdl                   -- ^
                  -> (Double,Double)       -- ^
                  -> Double                -- ^ Radisu der Automaten in der GUI
                  -> [Int]
   
   setStatePos    :: mdl                   -- ^ 
                  -> Int                     -- ^ Der Zustand
                  -> (Double,Double)       -- ^ Die neue Position
                  -> mdl                   -- ^
   
   getStatePos    :: mdl                   -- ^
                  -> Int                     -- ^ Der Zustand
                  -> Maybe (Double,Double) -- ^ Die Position, falls das Zustand existiert
                  
   setStateStart  :: mdl                   -- ^ 
                  -> Int                     -- ^ Der Zustand
                  -> Bool                  -- ^ Gibt an ob es sich um den Startzustand handelt
                  -> mdl                   -- ^
   
   getStateStart  :: mdl                   -- ^
                  -> Maybe Int             -- ^ Der Zustand
   
   setStateFinal  :: mdl                   -- ^ 
                  -> Int                     -- ^ Der Zustand
                  -> Bool                  -- ^ Gibt an ob es sich um einen Finalen Zustand handelt
                  -> mdl                   -- ^
   
   getStateFinal  :: mdl                   -- ^
                  -> [Int]                 -- ^ Finalen Zustände
               
   addState       :: mdl                   -- ^
                  -> Int                     -- ^ Zustand
                  -> (Double,Double)       -- ^ Die Position
                  -> mdl                   -- ^
               
   removeState    :: mdl                   -- ^
                  -> Int                   -- ^ Zustand
                  -> mdl                   -- ^ 
               
   removeEdge     :: mdl                   -- ^ 
                  -> Int                   -- ^ Der Start-Zustand
                  -> Int                   -- ^ Der Ziel-Zustand
                  -> Char                  -- ^ Bzgl Eingabesymbol
                  -> mdl                   -- ^
   
   addEdge        :: mdl                   -- ^ 
                  -> Int                   -- ^ Der Start-Zustand
                  -> Int                   -- ^ Der Ziel-Zustand
                  -> Char                  -- ^ Die neue Position
                  -> String                -- ^ Die neue Ausgabe (Wenn Transducer)
                  -> mdl                   -- ^
   
   getStates      :: mdl                       -- ^ 
                  -> [Int]   -- ^ Liste an Zustanden, mit Position
               
   getEdges       :: mdl                       -- ^ 
                  -> [(Int,Int, Char, String)] -- ^ Liste an Zustanden, mit Position
   