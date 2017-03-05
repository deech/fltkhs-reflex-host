{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Main where
import Data.Text as T
import Reflex as Reflex
import Reflex.Host.Class (EventTrigger, newEventWithTriggerRef, runHostFrame, fireEvents)
import Control.Monad(forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Ref as MonadRef
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Dependent.Sum (DSum ((:=>)))
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering))
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
  return $ fmap Prelude.reverse $ current d

host :: (forall t m. TypingApp t m) -> IO ()
host myGuest =
  runSpiderHost $ do
    (e, eTriggerRef) <- newEventWithTriggerRef
    mETriggerRef <- liftIO (readIORef eTriggerRef)
    let handler :: FL.Ref DoubleWindow -> FL.Event -> IO (Either UnknownEvent ())
        handler window fltkEvent =
          case fltkEvent of
            Keydown -> do
              keyPressed <- FL.eventText
              case mETriggerRef of
                Nothing -> return ()
                Just eTrigger ->
                  fireEvents [eTrigger :=> Identity (Prelude.head (T.unpack keyPressed))]
              return (Left UnknownEvent)
            _ -> handleSuper window fltkEvent
    window <- liftIO $ do
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
    liftIO $ do
      begin window
      description <- textDisplayNew  (toRectangle (30,35,478,90)) Nothing
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

    output <- liftIO $ do
                  o <- textDisplayNew (toRectangle (30,144,478,236)) Nothing
                  setLabel o "Output:"
                  setBox o BorderFrame
                  setColor o blackColor
                  setAlign o (Alignments [AlignTypeCenter,AlignTypeTop,AlignTypeLeft])
                  setWhen o [WhenNever]
                  wrapMode o WrapAtBounds
                  return o
    buffer <- liftIO $ do
      b <- textBufferNew Nothing Nothing
      setBuffer output (Just b)
      return b
    liftIO (end window)
    b <- runHostFrame (myGuest e)
    forever $ do
     userInput <- runHostFrame (sample b)
     liftIO $ setText buffer (T.pack userInput)
    liftIO $ do
      end window
      showWidget window
      _ <- FL.run
      return ()

main = host guest
