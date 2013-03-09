
import Data.Maybe
import Data.Tree
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
import Utils

--Settings:

drawRadius :: Double
drawRadius = 40.0

-- Code

main :: IO ()
main = do
    _           <- initGUI
    exit        <- newEmptyMVar
    
    -- "Variablen":
    automat     <- newTVarIO (Nothing::Maybe (VPT Char Char Int Char))
    atm         <- newTVarIO emptyGuiVpt -- Der Automat
    selectNode  <- newTVarIO (Nothing :: Maybe Int)
    clickNode   <- newTVarIO (Nothing :: Maybe Int)
    clickStart  <- newTVarIO (Nothing:: Maybe (Double,Double))
    clickEnd    <- newTVarIO (Nothing:: Maybe (Double,Double))
    
    Just xml    <- xmlNew "GUI.glade"    
    window      <- xmlGetWidget xml castToWindow "window1"
    drawingArea <- xmlGetWidget xml castToDrawingArea "drawingArea"
    
    txtCall     <- xmlGetWidget xml castToTextView "txtCall"
    txtReturn   <- xmlGetWidget xml castToTextView "txtReturn"
    txtInit     <- xmlGetWidget xml castToTextView "txtInit"
    
    -- init TreeView
    treeview <- xmlGetWidget xml castToTreeView "treeConn"
    list     <- listStoreNew ([]::[String])
    treeViewSetModel treeview list
    col      <- treeViewColumnNew
    treeViewColumnSetTitle col "Kanten:"
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer list
             $ \ind -> [cellText := ind]
    treeViewAppendColumn treeview col
    tree     <- treeViewGetSelection treeview
    treeSelectionSetMode tree  SelectionSingle

    -- Drawing Area
    drawingArea `widgetAddEvents` [ButtonPressMask,ButtonReleaseMask, ButtonMotionMask] 
    drawingArea `onExpose` (\_ -> renderScene drawingArea atm selectNode clickNode clickStart clickEnd automat)
    drawingArea `on` buttonPressEvent $ tryEvent $ drawingAreaPress atm selectNode clickNode clickStart clickEnd xml list
    drawingArea `on` buttonReleaseEvent $ tryEvent $ drawingAreaRelease atm clickNode clickStart clickEnd txtCall txtReturn txtInit
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
    
{-
 - Running
 -}
 
startRun = id
      
resetRun = id
    
{- 
 - Kanten Events
 -}
 
getTextFromTextView :: TextView 
                    -> IO String
getTextFromTextView view = do
       buffer <- textViewGetBuffer view
       sItr   <- textBufferGetStartIter buffer
       eItr   <- textBufferGetEndIter buffer
       str    <- textIterGetText sItr eItr
       return str

oneSelection :: forall cl. TreeViewClass cl
             => TreeStore String 
             -> TreeStore String 
             -> TreeSelection
             -> Button
             -> cl
             -> cl
             -> IO ()
oneSelection store storeRead select btn treeRead treeWrite  = do
   mItr <- treeSelectionGetSelected select
   when (isJust mItr) $ do
     path <- treeModelGetPath store (fromJust mItr)
     v <- treeStoreGetValue store path
     putStrLn $ " @ " ++ show path ++ show (length v) ++ "selected " ++ v
     when (length path == 2 ) $ do
        widgetSetSensitivity btn       True
        widgetSetSensitivity treeRead  False
        widgetSetSensitivity treeWrite False
        when (path!!0 == 0) $ do 
             widgetSetSensitivity treeRead    True
             select2 <- treeViewGetSelection  treeRead
             itr2 <- treeSelectionGetSelected select2
             when (isJust itr2) $ do
                path2 <- treeModelGetPath storeRead (fromJust itr2)
                print path2
        when (path!!0 == 1) $ widgetSetSensitivity treeWrite True
     unless (length path == 2 ) $ do
        widgetSetSensitivity btn       False
        widgetSetSensitivity treeRead  False
        rSel <- treeViewGetSelection treeRead
        treeSelectionUnselectAll rSel
        widgetSetSensitivity treeWrite False
        
getSelected :: TreeStore String -> TreeSelection -> IO (Maybe String)
getSelected str sel =  do
     itr <- treeSelectionGetSelected sel
     if isNothing itr then return Nothing
       else do
         path <- treeModelGetPath str (fromJust itr)
         value <- treeStoreGetValue str path
         return (Just value)

prepare :: String 
        -> String
prepare inp = foldl (\str c -> replace [c] ("\'" ++ [c] ++ "\'")  str) inp "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Diese Funktion konfiguriert den Tree für das Eingabesymbol
prepareTreeInput :: forall self. TreeViewClass self 
                 => self
                 -> self
                 -> self
                 -> Button
                 -> TreeStore String
                 -> TreeStore String
                 -> Tree String
                 -> Tree String
                 -> Tree String
                 -> IO (TreeStore String)
prepareTreeInput treeInput treeRead treeWrite btnOK strRead strWrite mCall mReturn mInit = do
    forest <- treeStoreNew [mCall , mReturn , mInit]    
    col <- treeViewColumnNew
    treeViewColumnSetTitle col "Symbole"
    treeViewSetModel treeInput forest
    treeViewSetHeadersVisible treeInput False
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer forest
           $ \ind -> [cellText := ind]           
    treeViewAppendColumn treeInput col
    tree <- treeViewGetSelection treeInput
    treeSelectionSetMode tree  SelectionBrowse
    onSelectionChanged tree (oneSelection forest strRead tree btnOK  treeRead treeWrite )
    return forest
    
    
prepareTreeRead :: forall self. TreeViewClass self 
                => self 
                -> IO (TreeStore [Char])
prepareTreeRead treeRead  = do
    forest <- treeStoreNew $ map (\c -> Node [c] []) "abcdeefghijklmnopqrstuvwxyz" 
    col <- treeViewColumnNew
    treeViewColumnSetTitle col "Symbole"
    treeViewSetModel treeRead forest
    treeViewSetHeadersVisible treeRead False
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer forest
           $ \ind -> [cellText := ind]           
    treeViewAppendColumn treeRead col
    tree <- treeViewGetSelection treeRead
    treeSelectionSetMode tree  SelectionBrowse
    --onSelectionChanged tree (oneSelection forest tree btnOK)
    return forest
    
prepareTreeWrite :: forall self. TreeViewClass self 
                 => self 
                 -> IO (TreeStore [Char])
prepareTreeWrite treeWrite = do
    forest <- treeStoreNew $ map (\c -> Node [c] []) "abcdeefghijklmnopqrstuvwxyz"   
    col <- treeViewColumnNew
    treeViewColumnSetTitle col "Symbole"
    treeViewSetModel treeWrite forest
    treeViewSetHeadersVisible treeWrite False
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer forest
           $ \ind -> [cellText := ind]           
    treeViewAppendColumn treeWrite col
    tree <- treeViewGetSelection treeWrite
    treeSelectionSetMode tree  SelectionBrowse
    --onSelectionChanged tree (oneSelection forest tree btnOK)
    return forest

edgeDialog :: TVar GUIVPT 
           -> GUIVPT
           -> Int
           -> Int
           -> TextView
           -> TextView
           -> TextView
           -> IO ()
edgeDialog tVar vpt p q txtCall txtReturn txtInit = do
    Just xml   <- xmlNew "Kante.glade"   
    lblName     <- xmlGetWidget xml castToLabel "lblName"
    dialog     <- xmlGetWidget xml castToDialog "kantenDialog"
    treeInput  <- xmlGetWidget xml castToTreeView "treeInput"
    treeRead   <- xmlGetWidget xml castToTreeView "treeStackRead"
    treeWrite  <- xmlGetWidget xml castToTreeView "treeStackWrite"
    btnOK      <- xmlGetWidget xml castToButton "btnOK"
    
    labelSetText lblName $ "Kante von " ++ show p ++ " zu " ++ show q
    
    (strCall::String)   <- do { str <- getTextFromTextView txtCall   ; return . read $ "[" ++ prepare str ++ "]" }
    (strReturn::String) <- do { str <- getTextFromTextView txtReturn ; return . read $ "[" ++ prepare str ++ "]" }
    (strInit::String)   <- do { str <- getTextFromTextView txtInit   ; return . read $ "[" ++ prepare str ++ "]" }
    mCall   <- return . Node "Call"   $ map (\c -> Node [c] []) strCall
    mReturn <- return . Node "Return" $ map (\c -> Node [c] []) strReturn
    mInit   <- return . Node "Init"   $ map (\c -> Node [c] []) strInit
    
    strRead  <- prepareTreeRead  treeRead
    strWrite <- prepareTreeWrite treeWrite
    strInput <- prepareTreeInput treeInput treeRead treeWrite btnOK strRead strWrite mCall mReturn mInit
    
    -----
    widgetShowAll dialog
    diares <- dialogRun dialog
    print diares
    when (diares == ResponseUser 0) $ do
         selInpt <- treeViewGetSelection treeInput
         selCall <- treeViewGetSelection treeRead
         selRetu <- treeViewGetSelection treeWrite
         Just inpSel <- getSelected strInput selInpt
         callSel <- getSelected strRead selCall
         retSel <- getSelected strWrite selRetu
         atomically $ writeTVar tVar ( addEdge vpt p q (head inpSel) (maybeHead callSel) (maybeHead retSel) (Just "Transduce THIS!") ) 
    widgetDestroy dialog
    return ()

maybeHead :: Maybe String -> Maybe Char
maybeHead Nothing      = Nothing
maybeHead (Just [])    = Just '+'
maybeHead (Just (x:_)) = Just x

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
                   -> TextView
                   -> TextView
                   -> TextView
                   -> EventM EButton ()
drawingAreaRelease vvptVar tNode tStart tEnd txtCall txtReturn txtInit = do
      p   <- eventCoordinates
      btn <- eventButton
      liftIO $ do 
        mnode <- atomically $ readTVar tNode
        vvpt  <- atomically $ readTVar vvptVar
        if (not $ null (getStatesOnPos vvpt p drawRadius)) && isJust mnode then do
          znode <- return $ head (getStatesOnPos vvpt p drawRadius)
          node  <- return $ fromJust mnode 
          if btn == RightButton then do 
              edgeDialog vvptVar vvpt node znode txtCall txtReturn txtInit
          else return ()              
          else do
          if btn == RightButton then do
            atomically $ writeTVar vvptVar (addState vvpt (maxIndex vvpt + 1) p)
          else if btn == LeftButton && isJust mnode then do
            node  <- return $ fromJust mnode 
            atomically $ writeTVar vvptVar (setStatePos vvpt node p)
            else return ()         
        atomically $ writeTVar tNode  Nothing
        atomically $ writeTVar tStart Nothing
        atomically $ writeTVar tEnd   Nothing
           
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
        
{-
 - Rendern
 -}

renderScene ::  DrawingArea 
            -> TVar GUIVPT 
            -> TVar (Maybe Int)
            -> TVar (Maybe Int)
            -> TVar (Maybe (Double, Double))
            -> TVar (Maybe (Double, Double))
            -> TVar (Maybe (VPT Char Char Int Char))
            -> IO Bool
renderScene da vvptVar selNode tNode tStart tEnd automat = do
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
          (p,q) <- if b then return ((z + sintan (abs(x-z)) (abs(y-w))*signum(x-z)),(w + costan (abs(x-z)) (abs(y-w))*signum(y-w))) else return (z,w)
          lineTo p q
          lineTo (p-sin(330.0/180.0*pi)*20) (q+cos(330.0/180.0*pi)*20) 
          moveTo p q
          lineTo (p-sin(30.0/180.0*pi - sinh((x-p)/(y-w)) )*20) (q+cos(30.0/180.0*pi - cosh((x-p)/(y-w)))*20)
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

