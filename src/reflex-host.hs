{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Main where
import Data.Text as T
import Reflex as Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS as FL
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FL
type TypingApp t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Reflex.Event t Char
                  -> m (Behavior t String)

guest :: TypingApp t m
guest e = do
  d <- foldDyn (:) [] e
  return $ fmap Prelude.reverse $ (current d)

makeWindow :: (FL.Ref DoubleWindow -> FL.Event -> IO (Either UnknownEvent ())) -> IO (FL.Ref DoubleWindow)
makeWindow handler = do
  w <- doubleWindowCustom
         (toSize (538,413))
         (Just (toPosition (113,180)))
         (Just "FLTKHS Reflex Host Port")
         Nothing
         (defaultCustomWidgetFuncs
          {
            handleCustom = Just handler
          })
         defaultCustomWindowFuncs
  setColor w whiteColor
  setLabelfont w helveticaBold
  setVisible w
  return w

makeDescription :: IO ()
makeDescription = do
  description <- textDisplayNew (toRectangle (30,35,478,90)) Nothing
  setLabel description "Reflex Host Example"
  setBox description NoBox
  setLabelfont description helveticaBold
  setWhen description [WhenNever]
  setTextfont description courier
  setTextsize description (FontSize 12)
  wrapMode description WrapAtBounds
  dBuffer <- textBufferNew Nothing Nothing
  setText dBuffer "\n\nThis is a port of the 'host' example that ships with 'try-reflex'.\n\nType anywhere and the accumulated output will show up below."
  setBuffer description (Just dBuffer)

makeOutput :: IO (FL.Ref TextBuffer, FL.Ref TextDisplay)
makeOutput = do
  o <- textDisplayNew (toRectangle (30,144,478,236)) Nothing
  setLabel o "Output:"
  setBox o BorderFrame
  setTextfont o courier
  setTextsize o (FontSize 12)
  setWhen o [WhenNever]
  b <- textBufferNew Nothing Nothing
  setBuffer o (Just b)
  return (b,o)

outputChanged :: FL.Ref TextBuffer -> T.Text -> IO Bool
outputChanged buffer newText = do
  oldText <- getText buffer
  if (not (T.null oldText))
    then do
      let oldLastLine = Prelude.head (Prelude.reverse (T.lines oldText))
      return (newText /= oldLastLine)
    else return (not (T.null newText))

host :: (forall t m. TypingApp t m) -> IO ()
host myGuest =
  runSpiderHost $ do
    (e, eTriggerRef) <- newEventWithTriggerRef
    let windowHandler :: FL.Ref DoubleWindow -> FL.Event -> IO (Either UnknownEvent ())
        windowHandler window fltkEvent =
          case fltkEvent of
            Keydown -> do
              keyPressed <- FL.eventText
              eventTrigger <- liftIO (readIORef eTriggerRef)
              if (not (T.null keyPressed))
                then runSpiderHost $
                       case eventTrigger of
                          Nothing -> return ()
                          Just event ->
                            fireEvents [event :=> Identity (Prelude.head (T.unpack keyPressed))] >>
                            liftIO (return ())
                else return ()
              return (Right ())
            _ -> handleSuper window fltkEvent
    b <- runHostFrame (myGuest e)
    liftIO $ do
      w <- makeWindow windowHandler
      begin w
      makeDescription
      (buffer,o) <- makeOutput
      end w
      setResizable w (Just o)
      showWidget w
      go (do
           leftTodo <- FL.wait
           return (leftTodo > 0)
         )
         (runSpiderHost $ do
            output <- runHostFrame (sample b)
            liftIO $ do
              newOutput <- outputChanged buffer (T.pack output)
              if newOutput
                then do
                  emptyBuffer <- getText buffer >>= return . T.null
                  appendToBuffer buffer (T.pack (if emptyBuffer then output else ("\n" ++ output)))
                else return ())
  where
    go :: IO Bool -> IO ()  -> IO ()
    go predicateM action = do
      predicate <- predicateM
      if predicate
        then action >> go predicateM action
        else return ()

main :: IO ()
main = host guest
