{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Cases                       (snakify)
import           Control.Lens
import           Control.Monad.Fix
import           Data.Aeson
import           Data.Aeson.Text             (encodeToLazyText)
import           Data.Text                   (Text, replace)
import           Data.Text.Lazy              (toStrict)
import           Data.Word
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


infixr 4 ?+~, ?-~

{-# INLINE (?+~) #-}
(?+~) :: ASetter s t (Maybe Word) (Maybe Word) -> Word -> s -> t
l ?+~ n = l %~ addn
  where addn Nothing  = Nothing
        addn (Just m) = Just (m + n)

{-# INLINE (?-~) #-}
(?-~) :: ASetter s t (Maybe Word) (Maybe Word) -> Word -> s -> t
l ?-~ n = l %~ subn
  where subn Nothing = Nothing
        subn(Just m) | m >= n    = Just (m - n)
                     | otherwise = Just m

-- | In the following relations, Nothing is smaller than any Just number.
infix 4 ?<, ?>, ?<=, ?>=
{-# INLINE (?<) #-}
(?<) :: Ord a => Maybe a -> a -> Bool
Nothing  ?< _  = True
(Just x) ?< y  = x < y

{-# INLINE (?>) #-}
(?>) :: Ord a => Maybe a -> a -> Bool
Nothing  ?> _  = False
(Just x) ?> y  = x > y

{-# INLINE (?<=) #-}
(?<=) :: Ord a => Maybe a -> a -> Bool
Nothing  ?<= _  = True
(Just x) ?<= y  = x <= y

{-# INLINE (?>=) #-}
(?>=) :: Ord a => Maybe a -> a -> Bool
Nothing  ?>= _  = False
(Just x) ?>= y  = x >= y

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
