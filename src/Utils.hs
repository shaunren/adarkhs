{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Cases                       (snakify)
import           Control.Lens
import           Control.Monad.Fix
import           Data.Aeson
import           Data.Aeson.Text             (encodeToLazyText)
import           Data.Map                    (Map, isSubmapOfBy)
import           Data.Text                   (Text, replace)
import           Data.Text.Lazy              (toStrict)
import           Language.Javascript.JSaddle (FromJSString, JSM, JSString, JSVal,
                                              ToJSString,
                                              liftJSM,
                                              textToJSString, toJSString,
                                              toJSVal, fromJSVal)
import           JSDOM                       (currentWindow)
import           JSDOM.Storage               (getItem, setItem)
import           JSDOM.Window                (getLocalStorage)
import           JSDOM.Types                 (Nullable(..), nullableToMaybe)
import           Reflex
import           System.Random               (RandomGen, randomR)
import           TextShow

{-# INLINE monoidGuard #-}
monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty

-- | Bounded enum successor
{-# INLINE succ' #-}
succ' :: (Eq a, Enum a, Bounded a) => a -> a
succ' x
  | x == maxBound = x
  | otherwise     = succ x

-- | Bounded enum predecessor
{-# INLINE pred' #-}
pred' :: (Eq a, Enum a, Bounded a) => a -> a
pred' x
  | x == minBound = x
  | otherwise     = pred x

{-# INLINE jsonEncode #-}
jsonEncode :: ToJSON a => a -> JSM JSString
#ifdef ghcjs_HOST_OS
jsonEncode = fmap js_stringify . toJSVal . toJSON
foreign import javascript unsafe "JSON.stringify($1)"
  js_stringify :: JSVal -> JSString
#else
jsonEncode = return . textToJSString . toStrict . encodeToLazyText
#endif


getLocalStorageItem :: FromJSString a => JSString -> JSM (Maybe a)
setLocalStorageItem :: ToJSString a => JSString -> a -> JSM ()

#ifdef ghcjs_HOST_OS

{-# INLINE getLocalStorageItem #-}
getLocalStorageItem key = do
  Nullable jsval <- getLocalStorageItem_ key
  nullableToMaybe jsval

{-# INLINE setLocalStorageItem #-}
setLocalStorageItem key val = setLocalStorageItem_ key (toJSString val)

foreign import javascript unsafe "window.localStorage.getItem($1)"
  getLocalStorageItem_ :: JSString -> JSM (Nullable JSString)

foreign import javascript unsafe "window.localStorage.setItem($1, $2);"
  setLocalStorageItem_ :: JSString -> JSString -> JSM ()

#else

getLocalStorageItem key = do
  Just window <- currentWindow
  ls <- getLocalStorage window
  getItem ls key

setLocalStorageItem key val = do
  Just window <- currentWindow
  ls <- getLocalStorage window
  setItem ls key val

#endif

-- | Converts a text of form "SomeDataType" to "some data type".
{-# INLINE toSpaceCase #-}
toSpaceCase :: Text -> Text
toSpaceCase = replace "_" " " . snakify

-- | Converts a data row key to output form.
{-# INLINE showRowKey #-}
showRowKey :: TextShow a => a -> Text
showRowKey = toSpaceCase . showt

-- | A Bool Dynamic that indicates whether the event has fired at least once
{-# INLINABLE firedOnce #-}
firedOnce :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Dynamic t Bool)
firedOnce ev = foldDynMaybe (\_ alreadyFired -> if alreadyFired then Nothing else Just True) False ev


infix 4 .<=.
{-# INLINE (.<=.) #-}
(.<=.) :: (Ord k, Ord v) => Map k v -> Map k v -> Bool
(.<=.) = isSubmapOfBy (<=)

-- Die roll that explodes on the largest value.
explodingDie :: RandomGen g => [Int] -> g -> (Int, g)
explodingDie [] g = (0,g)
explodingDie (n:ns) g =
  let (x,g1) = randomR (1,n) g
  in if x == n
     then let (x',g2) = explodingDie ns g1 in (x+x', g2)
     else (x,g1)
