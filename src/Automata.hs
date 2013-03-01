{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

{-| Automaten Typklassen
 - Author: Kai Sauerwald
 - Die Arbeit kann gerne unter Namensnennung ver�ndert und weiter gegeben werden.
 -}
module Automata where

{- | 
 - Diese Typklasse ist eine Zusammenfassung der Wichtigsten Eigenschaften von Automaten.
 - Für eine Implementierung sind 'accept' und 'run' optional.
 -}
class AutomataModell mdl where
    -- ^ Die Zustände des Automaten
    states  :: mdl q w  -- ^ Der Automat
            -> [q]      -- ^ Returnt die Liste an Zust�nden

    -- ^ Die Startzustände des Automaten
    start   :: mdl q w  -- ^ Der Automat
            -> q      -- ^ Returnt die Liste an Startzust�nden

    -- ^ Die akzeptierenden Zust�nde des Automanten
    final   :: mdl q w  -- ^ Der Automat
            -> [q]      -- ^ Returnt die Liste an akzeptierenden Zust�nden

    -- ^ Der Fehlerzustand bei eingabe-/übergangs- Fehlern
    failure :: mdl q w  -- ^ Der Automat
            -> q        -- ^ Der Fehlerzustand des Automaten
            
    -- ^ Gibt eine Startinstanz des Automaten zurück
    initial :: (Automata aut) 
            => mdl q w
            -> aut p z 
            
{- |
  - Diese Typklasse ist für Automatenrepräsentationen
  -}
class (AutomataModell aut) => Automata aut where
    -- ^ Der Zustanden in dem sich der Automat befindet.
    state   :: aut q w  -- ^ Der Automat
            -> q        -- ^ Der Zustand
            
    -- ^ Ist der Automat in einem Finalen Zustand
    isFinal :: (Eq q)
            => aut q w  -- ^ der Automat
            -> Bool     -- ^ True wenn Final, False anderernfalls
    isFinal aut = state aut `elem` final aut 

    -- ^ Stellt fest ob es einen akzeptienden Lauf für das Wort w gibt
    accept  :: (Eq q) 
            => aut q w  -- ^ Der Automat
            -> [w]      -- ^ Das Eingabewort w
            -> Bool     -- ^ Returnt True wenn es einen akzeptierenden Lauf gibt f�r das Wort w von den Startzust�nden aus.
    accept aut wx = isFinal $ stepE aut wx

    -- ^ Die fortführung von 'step' auf Wörter
    stepE   :: aut q w  -- ^ Der Automat
            -> [w]      -- ^ Das Eingabewort w
            -> aut q w  -- ^ Der erreichte Automat
    stepE = foldl step

    -- ^ Führt eine Zustandsüberführung aus.
    step    :: aut q w  -- ^ Der Automat
            -> w        -- ^ Das Eingabesymbol
            -> aut q w  -- ^ Der erreichte Zustand
    
    -- ^ Führt einen \"run\" aus, gibt alle zust�nde zur�ck.
    run     :: aut q w  -- ^ Der Automat
            -> [w]      -- ^ Das Eingabewort
            -> [aut q w]-- ^ Der \"run\"
    run aut []     = [aut]
    run aut (w:wx) = aut : run (step aut w) wx
   

{-| 
 - Diese Typeklasse fasst die Eigenschaften von Transducern einmal zusammen.
 - Für eine Implementierung ist 'runAndTransduce' optional.
 -}
class Transducer tds where
    transduce       :: Automata (tds o) -- Der Automat wird erzwungen
                    => (tds o) q w      -- ^ Der Automat
                    -> [w]              -- ^ Eingabewort
                    -> [o]              -- ^ Ausgabewort
    transduce _  []      = []
    transduce tds (w:wx) = out : transduce naut wx
          where (naut,out) = transStep tds w

    runAndTransduce :: Automata (tds o) -- Der Automat wird erzwungen
                    => tds o q w        -- ^ Der Automat
                    -> [w]              -- ^ Eingabewort
                    -> ([tds o q w], [o])        -- ^ Ausgabe-Zustand und Wort
    runAndTransduce aut w = (run aut w,transduce aut w)

    transStep       :: Automata (tds o) -- Der Automat wird erzwungen
                    => tds o q w        -- ^ Der Automat
                    -> w                -- ^ Eingabesymbol
                    -> (tds o q w,o)    -- ^ Ausgabe-Zustand und Symbol
