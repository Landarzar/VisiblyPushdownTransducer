
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
import GUIAutomata
import GUIVPT

--Settings:

drawRadius :: Double
drawRadius = 40.0

-- Code

main :: IO ()
main = do
    _      <- initGUI
    exit   <- newEmptyMVar
    
    -- "Variablen":
    atm <- atomically $ newTVar $ emptyGuiVpt -- Der Automat
    selectNode  <- newTVarIO (Nothing :: Maybe Int)
    clickNode  <- newTVarIO (Nothing :: Maybe Int)
    clickStart <- newTVarIO (Nothing:: Maybe (Double,Double))
    clickEnd   <- newTVarIO (Nothing:: Maybe (Double,Double))
    
    Just xml    <- xmlNew "GUI.glade"    
    window      <- xmlGetWidget xml castToWindow "window1"
    drawingArea <- xmlGetWidget xml castToDrawingArea "drawingArea"
    
    -- init TreeView
    treeview <- xmlGetWidget xml castToTreeView "treeConn"
    list <- listStoreNew ([]::[String])
    treeViewSetModel treeview list
    col <- treeViewColumnNew
    treeViewColumnSetTitle col "Kanten:"
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer list
             $ \ind -> [cellText := ind]
    treeViewAppendColumn treeview col
    tree <- treeViewGetSelection treeview
    treeSelectionSetMode tree  SelectionSingle

    -- Drawing Area
    drawingArea `widgetAddEvents` [ButtonPressMask,ButtonReleaseMask, ButtonMotionMask] 
    drawingArea `onExpose` (\_ -> renderScene drawingArea atm selectNode clickNode clickStart clickEnd)
    drawingArea `on` buttonPressEvent $ tryEvent $ drawingAreaPress atm selectNode clickNode clickStart clickEnd xml list
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

edgeDialog :: TVar GUIVPT 
           -> GUIVPT
           -> Int
           -> Int
           -> IO ()
edgeDialog tVar vpt p q  = do
          dialog    <- dialogNew 
          set dialog [windowTitle := "Erstelle Kante"]
          lbl       <- labelNew (Just "Eingabesymbol")
          txtIn     <- entryNew
          lbl2      <- labelNew (Just "Stack Pop")
          txtStackR <- entryNew
          lbl3      <- labelNew (Just "Stack Push")
          txtStackC <- entryNew
          lbl4      <- labelNew (Just "Ausgabesymbol")
          txtOut    <- entryNew
          hbox      <- dialogGetUpper dialog
          listmdl   <- listStoreNew ["In1","In2","In3"]
          picker    <- iconViewNewWithModel listmdl
          
          col <- treeViewColumnNew
          treeViewColumnSetTitle col "Kanten:"
          renderer <- cellRendererTextNew
          cellLayoutPackStart col renderer False
          cellLayoutSetAttributes col renderer listmdl
                   $ \ind -> [cellText := ind]
          iconViewSetColumns picker 1
          
          boxPackStart hbox lbl       PackNatural 0
          boxPackStart hbox txtIn     PackNatural 1
          boxPackStart hbox lbl2      PackNatural 2
          boxPackStart hbox txtStackR PackNatural 3
          boxPackStart hbox lbl3      PackNatural 4
          boxPackStart hbox txtStackC PackNatural 5
          boxPackStart hbox lbl4      PackNatural 6
          boxPackStart hbox txtOut    PackNatural 7
          boxPackStart hbox picker    PackGrow 8
          btnJa     <- dialogAddButton dialog "Erstellen" ResponseAccept
          btnNo     <- dialogAddButton dialog "Abbrechen" ResponseNo 
          widgetShowAll hbox
          diares    <- dialogRun dialog
          inText    <- entryGetText txtIn
          stackRText <- entryGetText txtStackR
          stackCText <- entryGetText txtStackC
          outText   <- entryGetText txtOut
          when (diares == ResponseAccept) $ do
             atomically $ writeTVar tVar (addEdge vpt p q (head inText) (if null stackRText then Nothing else Just (head stackRText)) (if null stackCText then Nothing else Just (head stackCText)) (if null outText then Nothing else Just outText))
          widgetDestroy dialog
          return ()

{-  
 -  Events der DrawingArea
 -}

-- | Diese Funktion wird Aufgerufen wenn der
drawingAreaPress :: TVar GUIVPT
                 -> TVar (Maybe Int)
                 -> TVar (Maybe Int)
                 -> TVar (Maybe (Double, Double))
                 -> TVar (Maybe (Double, Double))
                 -> GladeXML
                 -> ListStore String
                 -> EventM EButton ()
drawingAreaPress avar slcNode tNode tStart tEnd xml list = do
      p     <- eventCoordinates
      btn   <- eventButton
      click <- eventClick 
      liftIO $ do
        vvpt  <- atomically $ readTVar avar    -- Der Automat
        when (click == SingleClick) $ do
          when (not $ null (getStatesOnPos vvpt p drawRadius)) $ do
            nID <- return (listToMaybe (getStatesOnPos vvpt p drawRadius))
            when (btn == RightButton) $ do 
              atomically $ writeTVar tNode nID
              atomically $ writeTVar tStart (Just p)
              atomically $ writeTVar tEnd (Just p)             
            unless (btn == RightButton) $ do
              atomically $ writeTVar tNode nID
              atomically $ writeTVar tStart Nothing
              atomically $ writeTVar tEnd Nothing
        when (click == DoubleClick) $ do
          nID <- return (listToMaybe (getStatesOnPos vvpt p drawRadius))
          selectState vvpt slcNode xml list nID

selectState :: GUIVPT -> TVar (Maybe Int) ->  GladeXML -> ListStore String -> Maybe Int -> IO ()
selectState vvpt slcNode xml list (Nothing) = do
                    lblName <- xmlGetWidget xml castToLabel "lblName"
                    cbtnFinal <- xmlGetWidget xml castToCheckButton "cbtnFinal"
                    cbtnStart <- xmlGetWidget xml castToCheckButton "cbtnStart"
                    txtOut <- xmlGetWidget xml castToEntry "txtOut"
                    txtIn <- xmlGetWidget xml castToEntry "txtIn"
                    labelSetLabel lblName "-1"
                    toggleButtonSetActive cbtnFinal False
                    toggleButtonSetActive cbtnStart False
                    listStoreClear list
                    atomically $ writeTVar slcNode Nothing
                    return ()
selectState vvpt slcNode xml list (Just id) = do
                    lblName <- xmlGetWidget xml castToLabel "lblName"
                    cbtnFinal <- xmlGetWidget xml castToCheckButton "cbtnFinal"
                    cbtnStart <- xmlGetWidget xml castToCheckButton "cbtnStart"
                    txtOut <- xmlGetWidget xml castToEntry "txtOut"
                    txtIn <- xmlGetWidget xml castToEntry "txtIn"
                    treeview <- xmlGetWidget xml castToTreeView "treeConn"
                    labelSetLabel lblName $ show id
                    when (isJust $ getStateStart vvpt) $ toggleButtonSetActive cbtnFinal (id == (fromJust $ getStateStart vvpt))
                    toggleButtonSetActive cbtnStart (id `elem` getStateFinal vvpt)
                    listStoreClear list
                    mapM_ (f list) (getEdges vvpt)
                    atomically $ writeTVar slcNode (Just id)
                    return ()
                 where f list z@(i,j,_,_,_,_) = do
                        when (i == id || j == id) $ do
                           listStoreAppend list $ show z
                           return ()
                          
       
drawingAreaRelease :: TVar GUIVPT
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
        if (not $ null (getStatesOnPos vvpt p drawRadius)) && isJust mnode then do
          znode <- return $ head (getStatesOnPos vvpt p drawRadius)
          node  <- return $ fromJust mnode 
          if btn == RightButton then do 
              edgeDialog vvptVar vvpt znode node
          else return ()              
          else do
          if btn == RightButton then do
            atomically $ writeTVar vvptVar (addState vvpt (maxIndex vvpt + 1) p)
          else if btn == LeftButton && isJust mnode then do
            node  <- return $ fromJust mnode 
            atomically $ writeTVar vvptVar (setStatePos vvpt node p)
            else return ()         
        atomically $ writeTVar tNode $ Nothing
        atomically $ writeTVar tStart Nothing
        atomically $ writeTVar tEnd Nothing
           
drawingAreaMotion :: TVar GUIVPT
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

renderScene ::  DrawingArea 
            -> TVar GUIVPT 
            -> TVar (Maybe Int)
            -> TVar (Maybe Int)
            -> TVar (Maybe (Double, Double))
            -> TVar (Maybe (Double, Double))
            -> IO Bool
renderScene da vvptVar selNode tNode tStart tEnd = do
    win   <- widgetGetDrawWindow da
    mnode <- atomically $ readTVar tNode
    mstart<- atomically $ readTVar tStart
    mend  <- atomically $ readTVar tEnd
    vvpt  <- atomically $ readTVar vvptVar
    slct  <- atomically $ readTVar selNode
    renderWithDrawable win $ do 
      renderAutomata da slct vvpt
      when (isJust mend && isJust mnode) $ do
        node    <- return $ fromJust mnode
        (xq,yq) <- return $ fromJust mend
        if isJust mstart then do
          (xp,yp) <- return $ fromJust $ getStatePos vvpt node
          renderConn (xp,yp) (xq,yq) False
        else do
          (xp,yp) <- return . fromJust $ getStatePos vvpt node
          setSourceRGBA 0.0 0.2 0.2 0.7 
          arc xq yq drawRadius 0.0 (2*pi)
          moveTo xq yq
          showText . show $ node
          moveTo xp (yp+drawRadius)  
          stroke
          closePath
      return True

renderConn :: (Double,Double) -> (Double,Double) -> Bool -> Render ()
renderConn (x,y) (z,w) b = do
          moveTo   (x + sintan (abs(x-z)) (abs(y-w))*signum(z-x)) (y + costan (abs(x-z)) (abs(y-w))*signum(w-y))
          when b $ do
            lineTo (z + sintan (abs(x-z)) (abs(y-w))*signum(x-z)) (w + costan (abs(x-z)) (abs(y-w))*signum(y-w))
          unless b $ do
            lineTo z w
          stroke
          closePath
          where sintan,costan :: Double -> Double -> Double
                sintan a b = (sin . tanh) (a/b) * drawRadius
                costan a b = (cos . tanh) (a/b) * drawRadius

renderAutomata :: DrawingArea -> Maybe Int -> GUIVPT -> Render ()
renderAutomata _ slect vvpt = do 
     mapM_ dornd $ gvPos vvpt
     mapM (f) (getEdges vvpt)
     stroke
     return ()
       where dornd :: (Int,(Double,Double)) -> Render ()
             dornd (a,(x,y)) = do
               -- Zeichne Zustände
               when (isJust slect) $ when (fromJust slect == a) $ do { setSourceRGBA 1.0 0.2 0.2 0.7 }
               if a `elem` (getStateFinal vvpt) then setLineWidth 3.5 else setLineWidth 2.5
               --if a `elem` (final vpt) then setDash [4] 0 else setDash [] 0
               when (isJust $ getStateStart vvpt) $ when (a == (fromJust $ getStateStart vvpt)) $ do
                 arc x y (drawRadius-5.0) 0.0 (2*pi)
                 stroke
               arc x y drawRadius 0.0 (2*pi)
               moveTo x y
               showText . show $ a
               stroke
               -- Zeichne Kanten
               setSourceRGBA 0 0 0 1 
             f :: (Int,Int,Char,Maybe Char, Maybe Char, Maybe String) -> Render ()
             f (i,j,_,_,_,_) = do
                   p <- return $ fromJust $ getStatePos vvpt i
                   q <- return $ fromJust $ getStatePos vvpt j
                   renderConn p q True

