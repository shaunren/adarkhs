{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Main where

import           Control.Lens
import           Control.Monad               (unless, when)
import           Control.Monad.Reader
import           Control.Monad.Writer.Stricter
import           Control.Monad.Writer.Class
import           Data.Aeson
import           Data.Default
import           Data.FileEmbed
import qualified Data.Map                    as M
import           Data.Map                    ((!))
import           Data.Maybe                  (fromMaybe, fromJust, listToMaybe, isJust)
import           Data.Monoid                 ((<>))
import qualified Data.Set                    as S
import           Data.Text                   (Text, toLower)
import           Data.Time.Clock
import           Foreign.JavaScript.Utils    (jsonDecode)
import           JSDOM                       (currentWindow)
import           JSDOM.Storage               (getItem, setItem)
import           JSDOM.Window                (getLocalStorage)
import           Language.Javascript.JSaddle (JSM, JSString, JSVal, liftJSM,
                                              textToJSString, toJSVal)

import           Reflex.Dom                  hiding (button)


import           State
import           Utils
import           Widgets

--------------------------------------------------------------------------------------------------------------------------------------
describeRoom :: FireLevel -> Text
describeRoom fl
    | fl == minBound = "A Dark Room"
    | otherwise      = "A Firelit Room"

describeVillage :: Buildings -> Text
describeVillage bs
  | numHuts <= 0  = "A Silent Forest"
  | numHuts == 1  = "A Lonely Hut"
  | numHuts <= 4  = "A Tiny Village"
  | numHuts <= 8  = "A Modest Village"
  | numHuts <= 14 = "A Large Village"
  | otherwise     = "A Raucous Village"
  where numHuts = bs ^. at Hut . non 0

notify :: (Reflex t, NotifyShow a) => Event t a -> Event t [Text]
notify ev = ffor ev $ \a -> [showN a]

notifyDyn :: (Reflex t, NotifyShow a) => Dynamic t a -> Event t [Text]
notifyDyn dynA = notify $ updated dynA

coolFire :: GameAction
coolFire state  = return $ state & fireLevel %~ pred'

stokeFire :: GameAction
stokeFire state =
  if hasEnoughWood
    then do
      when (state ^. timesFireStoked == 0) $
        tell ["the light from the fire spills from the windows, out into the dark."]
      return $ state & fireLevel       .~ fl'
                     & timesFireStoked +~ 1
                     & stores          %~ updateWood
    else tell ["the wood has run out."] >> return state

  where
    villageUnlocked = Village `S.member` (state^.allowedLocations)
    hasEnoughWood = (not villageUnlocked) -- Stoking fire is free before Village is unlocked.
                 || wood >= woodRequired

    fl = state^.fireLevel
    wood = state^.stores . at Wood . non 0
    fl' | fl == FireDead = FireBurning
        | otherwise      = succ' fl

    updateWood st
      | villageUnlocked && hasEnoughWood = st & at Wood %~ (\(Just w) -> Just (w-woodRequired))
      | otherwise                        = st

    -- Pieces of wood required to light/stoke fire
    woodRequired = if fl == FireDead then 5 else 1


adjustRoomTemp :: GameAction
adjustRoomTemp state = return $ state & roomTemp %~ updateTemp (state^.fireLevel)
  where
    updateTemp fl t
      | (fromEnum t) > (fromEnum fl) = pred' t
      | (fromEnum t) < (fromEnum fl) = succ' t
      | otherwise                    = t


adjustBuilderLevel :: GameAction
adjustBuilderLevel state =
  let bl' | (state^.fireLevel) >= FireFlickering && bl <= BuilderCollapsed = succ' bl
          | (state^.roomTemp) >= Warm && bl > BuilderCollapsed             = succ' bl
          | otherwise                                                      = bl
  in return $ state & builderLevel .~ bl'
  where
    bl  = state^.builderLevel

unlockVillage :: GameAction
unlockVillage state = do
  tell ["the wood is running out.", "the wind howls outside."]
  return $ state & allowedLocations %~ S.insert Village
                 & stores . at Wood ?~ 4

arriveVillage :: GameAction
arriveVillage state = do
  unless (state^.seenVillage) $
    tell ["the sky is grey and the wind blows relentlessly."]
  return $ state & seenVillage .~ True

gatherWood :: GameAction
gatherWood state = do
  tell ["dry bush and dead branches litter the forest floor."]
  return $ state & stores . at Wood . non 0 +~ woodGathered

  where
    woodGathered = if (state^.buildings . at Cart . non 0) > 0 then 50 else 10
--------------------------------------------------------------------------------------------------------------------------------------
animationTickDt :: NominalDiffTime
animationTickDt = 1/20

-- | Saves GameState to LocalStorage.
saveGameState :: GameState -> JSM ()
saveGameState st = setLocalStorageItem "gameState" =<< jsonEncode st

-- | Loads GameState from LocalStorage. If not found, returns def.
loadGameState :: JSM GameState
loadGameState = do
  maybeState <- (jsonDecode =<<) <$> getLocalStorageItem "gameState"
  return $ fromMaybe def maybeState

--------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = mainWidgetWithCss css body
  where css = $(embedFile "css/style.css")

body :: MonadWidget t m => m ()
body = do
  t0 <- liftIO getCurrentTime
  cooldownTick <- tickLossy animationTickDt t0
  adjustRoomTempTick <- tickLossy adjustRoomTempDelay t0

  evPostBuild <- getPostBuild

  evInitState <- performEvent $ ffor evPostBuild $ const $ liftJSM $ do
    st <- liftJSM loadGameState
    return $ \_ -> return st


  elClass "div" "wrapper" $ do
    rec
      (isSelectedMap, evUserAction) <- fmap (_2 %~ fmapMaybe listToMaybe) $
        runGameT (GameConfig cooldownTick dynState) $
          tabDisplayDyn "header" "headerButton" "selected" "main" $ M.fromList
            [ (Room,    ( describeRoom <$> dynFireLevel
                        , constDyn True
                        , roomTab) )
            , (Village, ( dynState <&> (\s -> describeVillage $ s ^. buildings)
                        , (Village `S.member`) <$> dynAllowedLocations
                        , villageTab) )
            ]

      adjustBuilderTick <- tickLossyFrom adjustBuilderDelay t0 (updated dynStokedForFirstTime)
      (dynState, dynActionMsgs) <- fmap splitDynPure $ foldDyn foldFun (def, []) $ mergeWith (>=>)
        [ evUserAction
        , coolFire <$ evCoolFire
        , adjustRoomTemp <$ adjustRoomTempTick
        , adjustBuilderLevel <$ adjustBuilderTick
        , unlockVillage <$ evUnlockVillage
        -- NOTE: using updated dynCurLocation directly causes infinite loop
        , arriveVillage <$ ffilter id (updated $ isSelectedMap ! Village)

        , evInitState
        ]
      let evActionMsgs = ffilter (not . null) $ updated dynActionMsgs

      dynAllowedLocations <- dynField dynState allowedLocations
      dynBuilderLevel <- dynField dynState builderLevel

      evUnlockVillage <- delay unlockVillageDelay $ ffilter (== BuilderCollapsed) $ updated dynBuilderLevel

      -- Fires whenever firelevel updates, _or_ whenever fire is stoked.
      dynFireLevel <- fmap (fmap fst) . holdUniqDyn $ dynState <&> (\s -> (s^.fireLevel, s^.timesFireStoked))

      dynStokedForFirstTime <- holdUniqDyn $ dynState <&> (\s -> (s^.timesFireStoked) > 0)

      evCoolFire <- debounce coolFireDelay $ leftmost [updated dynFireLevel, evCoolFire]

    dynRoomTemp  <- dynField dynState roomTemp

    notificationsPane $ mergeWith (++)
      [
        notifyDyn dynFireLevel

      , notifyDyn dynRoomTemp
      , notifyDyn dynBuilderLevel

      , evActionMsgs
      ]

    elClass "div" "menu" $ do
      evSave <- menuButton "save."
      performEvent_ $ ffor (tagPromptlyDyn dynState evSave) $ liftJSM . saveGameState

  where
#ifdef DEBUG
    coolFireDelay       = 60
    adjustRoomTempDelay = 5
    adjustBuilderDelay  = 5
    unlockVillageDelay  = 5
#else
    coolFireDelay       = 5 * 60
    adjustRoomTempDelay = 30
    adjustBuilderDelay  = 30
    unlockVillageDelay  = 15
#endif

    foldFun :: (GameState -> Writer [Text] GameState) -> (GameState, [Text]) -> (GameState, [Text])
    foldFun f (st, _) = runWriter $ f st

    dynField :: (MonadWidget t m, Eq a) => Dynamic t GameState -> Getting a GameState a -> m (Dynamic t a)
    dynField dynState field = holdUniqDyn $ dynState <&> (^.field)

    menuButton :: MonadWidget t m => Text -> m (Event t ())
    menuButton lbl = do
      (e,_) <- elClass' "span" "menuButton" $ text lbl
      return $ () <$ domEvent Click e

roomTab :: MonadGame t m => m ()
roomTab = elClass "div" "location" $ do
  dynFireLevel <- asksGameState fireLevel
  dynStores    <- asksGameState stores

  evStokeFire <-
    button $ def & label        .~ fmap fireBtnLabel dynFireLevel
                 & cooldownSecs .~ fmap (\s -> if (s ^. at Wood . non 1) > 0 then stokeCooldown else 0) dynStores
  performAction evStokeFire stokeFire
  dynBuilderLevel   <- fmap (fmap (^.builderLevel)) $ asks (^.gameState)
  buttonCol (dynBuilderLevel <&> (>= BuilderWorking)) $ do
    el "h1" $ text "build:"

    craftButton $ def & buildingType   .~ Trap
                      & maxCraftAmount .~ 10
                      & availableMsg   ?~ "builder says she can make traps to catch any creatures that might still be alive out there."
                      & buildMsg       ?~ "more traps to catch more creatures."
                      & maxMsg         ?~ "more traps won't help now."
                      & craftCost      .~ (\b -> Wood =: (((b ^. at Trap . non 0) + 1) * 10))

    craftButton $ def & buildingType   .~ Cart
                      & maxCraftAmount .~ 1
                      & availableMsg   ?~ "builder says she can make a cart for carrying wood."
                      & buildMsg       ?~ "the rickety cart will carry more wood from the forest."
                      & craftCost      .~ const (Wood =: 30)

  buttonCol (constDyn False)  $ do
    el "h1" $ text "craft:"

    button $ def & label        .~ constDyn "torch"
                 & cooldownSecs .~ constDyn 3 -- TEST

  buttonCol (constDyn False) $ do
    el "h1" $ text "buy:"

    button $ def & label        .~ constDyn "scales"
                 & cooldownSecs .~ constDyn 3 -- TEST

  dynAllowedLocations <- asksGameState allowedLocations
  dynWidget ((Village `S.member`) <$> dynAllowedLocations) $ elClass "div" "storesCol" $
    storesFieldset "stores" "stores" =<< asksGameState stores

  return ()

  where
#ifdef DEBUG
    stokeCooldown = 1
#else
    stokeCooldown = 10
#endif

    buttonCol isVisible w = dynWidget isVisible $ elClass "div" "roomButtonCol" w >> return ()

    fireBtnLabel fl
      | fl == minBound = "light fire"
      | otherwise      = "stoke fire"

villageTab :: MonadGame t m => m ()
villageTab = elClass "div" "location" $ do
  evGatherWood <-
    button $ def & label        .~ constDyn "gather wood"
                 & cooldownSecs .~ constDyn gatherWoodCooldown
  performAction evGatherWood gatherWood

  elClass "div" "storesCol" $
    storesFieldset "stores" "stores" =<< asksGameState stores
  return ()

  where
#ifdef DEBUG
    gatherWoodCooldown = 1
#else
    gatherWoodCooldown = 60
#endif
