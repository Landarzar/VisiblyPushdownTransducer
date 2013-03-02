{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module VPT where 

import Data.Maybe
import Automata

{-|
 - Datentyp "Visibly Pushdown Transducer".
 - Die Sprachmange der Visibily Pushdown Autmata (VPA) sind eine echte Teilklasse der Kontextfreien Sprachen.
 - Sie haben gegenüber den Kellerautomaten den Vorteil das sie
 - unter allen typischen Boolschen Operationen Abgeschlossen.
 - Die VPAs arbeiten auf  einen "getaggten" Alphabet. Das Alphabet wird in 3 Klassen eingeteilt:
 -   1. Call Symbole
 -   2. Return Symbole
 -   3. Internal Symbole
 - Für jede dieser Eingabesymbole gibt es eigene Zustandsübergänge.
 - Bei den VPT (Visibly Pushdown Transducer) komme noch Ausgaben hinzu.
 - Das Besondere ist, das die Erzeugbaren Sprachen eine �bermenge der CFL ist.
 - Die 4 Typvariablen sind:
 -   1. stack - Der Datentyp für Stacksymbole
 -   2. out   - Der Datentyp der Ausgabesymbole
 -   3. state - Der Datentyp für die Zust�nde
 -   4. alph  - Datentyp des Eingabealphabet
 -}
data VPT stack out state alph =
     VPT { -- | Test ob das Symbol aus dem Alphabet ein Return-Symbol ist.
           vptIsReturn :: alph -> Bool,

           -- | Test ob das Symbol aus dem Alphabet ein Call-Symbol ist.
           vptIsCall   :: alph -> Bool,

           -- | Test ob das Symbol aus dem Alphabet ein Internal-Symbol ist.
           vptIsInit   :: alph -> Bool,

           -- | Die Zustände des Automaten
           vptStates   :: [state],

           -- | Die Start-Zustände des Automaten
           vptStart    :: Maybe state, 

           -- | Die akzeptierenden Zustände des Automaten
           vptFinal    :: [state],

           -- | Der Fehlerzustand.
           vptFail     :: state,

           -- | Das untere Kellersymbol
           vptBot      :: stack,

           -- | Das Nichtssymbol für die Ausgabe
           vptNull     :: out,
           
            -- | Übergangsfunktion für Call-Symbole
           vptCall     :: state -> alph -> (state, stack, [out]),

           -- | Übergangsfunktion für Return-Symbole
           vptReturn   :: state -> stack -> alph -> (state,[out]),

           -- | Übergangsfunktion für Internal-Symbole
           vptInit      :: state -> alph -> (state,[out]),
          
           -- | Momentaner Status
           vptState    :: Maybe state,
           
           -- | Momentaner Status
           vptStack    :: [stack]
         }

instance AutomataModell (VPT stack out) where
    states         = vptStates
    start          = vptStart 
    final          = vptFinal
    failure        = vptFail
    
instance Automata (VPT stack out) where
    step tds w = fst $ transStep tds w 

instance Transducer (VPT stack) where
    transStep tds w = if vptIsReturn tds w then
                        (VPT (vptIsReturn tds) (vptIsCall tds) (vptIsInit tds) (vptStates tds) (vptStart tds) (vptFinal tds) (vptFail tds) (vptBot tds) (vptNull tds) (vptCall tds) (vptReturn tds) (vptInit tds) rstate rstack,rout)
                      else if vptIsCall tds w then
                        (VPT (vptIsReturn tds) (vptIsCall tds) (vptIsInit tds) (vptStates tds) (vptStart tds) (vptFinal tds) (vptFail tds) (vptBot tds) (vptNull tds) (vptCall tds) (vptReturn tds) (vptInit tds) cstate cstack,cout)
                      else
                        (VPT (vptIsReturn tds) (vptIsCall tds) (vptIsInit tds) (vptStates tds) (vptStart tds) (vptFinal tds) (vptFail tds) (vptBot tds) (vptNull tds) (vptCall tds) (vptReturn tds) (vptInit tds) istate (vptStack tds),iout)
                      where istate = Just . fst $ vptInit tds (fromJust $ vptState tds) w
                            iout   = snd $ vptInit tds (fromJust $ vptState tds) w
                            rstate = Just . fst $ vptInit tds (fromJust $ vptState tds) w
                            rout   = snd $ vptInit tds (fromJust $ vptState tds) w
                            rstack = case vptStack tds of
                                       []     -> [vptBot tds]
                                       (_:[]) -> [vptBot tds]
                                       (w:wx) -> wx
                            cstate = Just . (\(a,_,_) -> a) $ vptCall tds (fromJust $ vptState tds) w
                            cout   = (\(_,_,a) -> a) $ vptCall tds (fromJust $ vptState tds) w
                            cstack = ((\(_,a,_) -> a) $ vptCall tds (fromJust $ vptState tds) w) : (vptStack tds)

