
import Data.Maybe
import Control.Concurrent
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import System.Exit
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM (Modifier(..))

import VPT
import Automata

data VVPT = VVPT { alphabet :: (String, String, String), pos :: [(Int, (Double,Double))], links :: [(Int,Int,Char,Char)] , ziele :: [Int], starte :: [Int] }

isInState :: VVPT -> (Double,Double) -> Maybe Int
isInState vvpt p = foldl fkt Nothing (pos vvpt)
   where fkt Nothing (i,q) = if incircle p q drawRadius then Just i else Nothing
         fkt (Just a) _    = Just a
         
-- | Diese Funktion gibt den Maximal Idx zurück
vptMaxIndex :: VVPT -> Int
vptMaxIndex vvpt = maximum (map fst (pos vvpt)++[0])
         
-- | Diese Funktion fügt eine Kante ein
vptInsertEdge :: VVPT  -- ^ Die Autenrepräsentation
            -> Int  -- ^ Startzustand
            -> Int  -- ^ Zielzustand
            -> Char -- ^ Eingabesymbol
            -> Char -- ^ Ausgabesymbol
            -> VVPT -- ^ Das neue Automatenmodell
vptInsertEdge vvpt i j e a = VVPT (alphabet vvpt) (pos vvpt) ((i, j, e, a) : links vvpt) (ziele vvpt) (starte vvpt) 

-- | Diese Funktion fügt einen Zustand ein.
vptAddState :: VVPT -> (Double,Double) -> (VVPT,Int)
vptAddState vvpt p = (VVPT (alphabet vvpt) ((m + 1, p) : pos vvpt) (links vvpt) (ziele vvpt) (starte vvpt),m+1)
        where m = vptMaxIndex vvpt 

-- | Diese Funktion gibt die Position eines State zurück
vptPos :: VVPT -> Int -> Maybe (Double,Double)
vptPos vvpt i = foldl myfkt Nothing (pos vvpt)
    where myfkt Nothing (j,p) = if i == j then Just p else Nothing
          myfkt (Just p) _    = Just p
          myftk _  _    = Nothing
          
-- | Diese Funktion ändert die Position eines States
vptMove :: VVPT -> Int -> (Double,Double) -> VVPT
vptMove vvpt i p = VVPT (alphabet vvpt) (map mf $ pos vvpt) (links vvpt) (ziele vvpt) (starte vvpt)
    where mf :: (Int, (Double,Double)) -> (Int, (Double,Double))
          mf (j,q) = if i == j then (j,p) else (j,q)

--Settings:

drawRadius :: Double
drawRadius = 40.0

incircle :: (Double,Double) -> (Double,Double) -> Double -> Bool
incircle (x,y) (xz,yz) r = (x-xz)*(x-xz) + (y - yz)*(y - yz) < r*r

-- Code

main :: IO ()
main = do
    _      <- initGUI
    exit   <- newEmptyMVar
    
    -- "Variablen":
    atm <- atomically $ newTVar $ VVPT ([],[],[]) [] [] [] [] -- Der Automat
    clickNode  <- newTVarIO (Nothing :: Maybe Int)
    clickStart <- newTVarIO (Nothing:: Maybe (Double,Double))
    clickEnd   <- newTVarIO (Nothing:: Maybe (Double,Double))
    
    Just xml    <- xmlNew "GUI.glade"    
    window      <- xmlGetWidget xml castToWindow "window1"
    drawingArea <- xmlGetWidget xml castToDrawingArea "drawingArea"

    drawingArea `widgetAddEvents` [ButtonPressMask,ButtonReleaseMask, ButtonMotionMask] 
    drawingArea `onExpose` (\_ -> renderScene drawingArea atm  clickNode clickStart clickEnd)
    drawingArea `on` buttonPressEvent $ tryEvent $ drawingAreaPress atm clickNode clickStart clickEnd
    drawingArea `on` buttonReleaseEvent $ tryEvent $ drawingAreaRelease atm clickNode clickStart clickEnd
    drawingArea `on` motionNotifyEvent $ tryEvent $ drawingAreaMotion atm drawingArea clickNode clickStart clickEnd
    
    window `onDestroy` onWindowDestroy exit
    
    timer <- forkIO $ do
      let printTime t = do {threadDelay 100000; postGUIAsync $ widgetQueueDraw drawingArea; printTime (mod (t+1) 1001)}
      printTime (0::Int)

    widgetShowAll window
    
    mainGUI
    killThread timer
    signal <- takeMVar exit
    exitWith signal 

-- | Diese Funktion wird Aufgerufen wenn der
drawingAreaPress :: TVar VVPT
                 -> TVar (Maybe Int)
                 -> TVar (Maybe (Double, Double))
                 -> TVar (Maybe (Double, Double))
                 -> EventM EButton ()
drawingAreaPress avar tNode tStart tEnd = do
      p  <- eventCoordinates
      btn <- eventButton
      liftIO $ do
        vvpt  <- atomically $ readTVar avar    -- Der Automat
        when (isJust (isInState vvpt p)) $ do
           nID <- return (isInState vvpt p)
           atomically $ writeTVar tNode nID
           when (btn == RightButton) $ do 
             atomically $ writeTVar tStart (Just p)
             atomically $ writeTVar tEnd (Just p)             
           unless (btn == RightButton) $ do
             atomically $ writeTVar tStart Nothing
             atomically $ writeTVar tEnd Nothing
       
drawingAreaRelease :: TVar VVPT
                 -> TVar (Maybe Int)
                 -> TVar (Maybe (Double, Double))
                 -> TVar (Maybe (Double, Double))
                 -> EventM EButton ()
drawingAreaRelease vvptVar tNode tStart tEnd = do
      p   <- eventCoordinates
      btn <- eventButton
      liftIO $ do 
        mnode <- atomically $ readTVar tNode
        vvpt  <- atomically $ readTVar vvptVar
        if isJust (isInState vvpt p) then do
          znode <- return $ fromJust (isInState vvpt p)
          node  <- return $ fromJust mnode 
          if btn == RightButton then do 
              atomically $ writeTVar vvptVar (vptInsertEdge vvpt znode node 'a' 'a')
          else return ()              
        else do
          if btn == RightButton then do
            atomically $ writeTVar vvptVar (fst $ vptAddState vvpt p)
          else if btn == LeftButton then do
            node  <- return $ fromJust mnode 
            atomically $ writeTVar vvptVar (vptMove vvpt node p)
            else return ()         
        atomically $ writeTVar tNode $ Nothing
        atomically $ writeTVar tStart Nothing
        atomically $ writeTVar tEnd Nothing
        
           
drawingAreaMotion :: TVar VVPT
                 -> DrawingArea
                 -> TVar (Maybe Int)
                 -> TVar (Maybe (Double, Double))
                 -> TVar (Maybe (Double, Double))
                 -> EventM EMotion ()
drawingAreaMotion _ drawingArea tNode _ tEnd = do
      p    <- eventCoordinates
      --mod  <- eventModifierAll
      liftIO $ do
        mNode <- atomically $ readTVar tNode
        if isJust mNode then do
          atomically $ writeTVar tEnd (Just p)
          postGUIAsync $ widgetQueueDraw drawingArea 
        else return ()
      
onWindowDestroy :: MVar ExitCode -> IO ()
onWindowDestroy exit = do { 
        print "END"; 
        putMVar exit ExitSuccess; mainQuit } 

renderScene ::  DrawingArea -> TVar VVPT 
                 -> TVar (Maybe Int)
                 -> TVar (Maybe (Double, Double))
                 -> TVar (Maybe (Double, Double))
                 -> IO Bool
renderScene da vvptVar tNode tStart tEnd = do
    win <- widgetGetDrawWindow da
    mnode <- atomically $ readTVar tNode
    mstart <- atomically $ readTVar tStart
    mend <- atomically $ readTVar tEnd
    vvpt  <- atomically $ readTVar vvptVar
    renderWithDrawable win $ do 
      renderAutomata da vvpt
      when (isJust mend && isJust mnode) $ do
        node    <- return $ fromJust mnode
        (xq,yq) <- return $ fromJust mend
        if isJust mstart then do
          (xp,yp) <- return $ fromJust mstart
          moveTo xp yp
          lineTo xq yq
          stroke
          closePath
        else do
          (xp,yp) <- return . fromJust $ vptPos vvpt node
          setSourceRGBA 0.0 0.0 0.0 0.7 
          arc xq yq drawRadius 0.0 (2*pi)
          moveTo xq yq
          showText . show $ node
          moveTo xp (yp+drawRadius)  
          stroke
          closePath
      return True
      

renderAutomata :: DrawingArea -> VVPT -> Render ()
renderAutomata _ vvpt = do 
     mapM_ dornd $ pos vvpt
     stroke
     return ()
       where dornd :: (Int,(Double,Double)) -> Render ()
             dornd (a,(x,y)) = do
               -- Zeichne Zustände
               if a `elem` (ziele vvpt) then setLineWidth 3.5 else setLineWidth 2.5
               --if a `elem` (final vpt) then setDash [4] 0 else setDash [] 0
               if a `elem` (starte vvpt) then do { arc x y (drawRadius-5.0) 0.0 (2*pi) ; stroke } else return ()
               arc x y drawRadius 0.0 (2*pi)
               moveTo x y
               showText . show $ a
               stroke
               -- Zeichne Kanten
               mapM (f) (links vvpt)
               return () 
               where 
                 f :: (Int,Int,Char,Char) -> Render ()
                 f (i,j,_,_) = do
                   moveTo (fst . fromJust $ vptPos vvpt i)  (snd . fromJust $ vptPos vvpt i)
                   lineTo (fst . fromJust $ vptPos vvpt j)  (snd . fromJust $ vptPos vvpt j)
                   closePath
                   stroke

