{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Main where

import           Control.Lens
import           Control.Monad               (unless, when)
import           Control.Monad.Random.Strict
import           Control.Monad.Reader
import           Control.Monad.Writer.Stricter
import           Control.Monad.Writer.Class
import           Data.Aeson
import           Data.Default
import           Data.FileEmbed
import           Data.List                   (foldl1')
import qualified Data.Map                    as M
import           Data.Map                    ((!))
import           Data.Maybe                  (fromMaybe, fromJust, listToMaybe, isJust)
import           Data.Monoid                 ((<>))
import qualified Data.Set                    as S
import           Data.Text                   (Text, toLower)
import           Data.Time.Clock
import           Foreign.JavaScript.Utils    (jsonDecode)
import           Language.Javascript.JSaddle (JSM, JSString, JSVal, liftJSM,
                                              textToJSString, toJSVal)
import           System.Random               (RandomGen, StdGen, newStdGen, split, randomR, random)

import           Reflex.Dom                  hiding (button)
import           TextShow

import           State
import           Utils
import           Widgets


--------------------------------------------------------------------------------------------------------------------------------------
hutCapacity :: Int
hutCapacity = 4

incomeTicks :: Int
incomeTicks = 10


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
coolFire state  =
  if state^.builderLevel >= BuilderWorking && state^.fireLevel <= FireFlickering && state^.stores . at Wood . non 0 > 0
  then do
    tell ["builder stokes the fire."]
    stokeFire state
  else
    return $ state & fireLevel %~ pred'

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
      | villageUnlocked && hasEnoughWood = st & at Wood . _Just -~ woodRequired
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
      st' = state & builderLevel .~ bl'
  in
    if bl' == BuilderWorking
      -- Builder helps gather wood
      then return $ st' & workers . at Builder ?~ (def & numWorkers .~ 1)
      else return st'
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

checkTraps :: GameAction
checkTraps state = do
  let
    Just rng = state^.randomGens . at RNGTrap
    (dropsList, rng') = flip runRand rng $ replicateM numDrops $ do
      roll <- getRandom
      let Just (_, item) = M.lookupGE roll rollCutoffMap
      return $ item =: 1

  let drops = foldl1' (M.unionWith (+)) dropsList

  -- Emit drop messages
  tell . map dropMsg $ M.keys drops

  return $ state & stores                  %~ M.unionWith (+) drops
                 & randomGens . at RNGTrap ?~ rng'
  where
    numTraps = state^.buildings . at Trap . non 0
    numBait  = state^.stores . at Bait . non 0
    numDrops = numTraps + min numBait numTraps

    rollCutoffMap :: M.Map Float StoreType =
      [ (0.5,   Fur)
      , (0.75,  Meat)
      , (0.85,  Scales)
      , (0.93,  Teeth)
      , (0.995, Cloth)
      , (1,     Charm)
      ]

    dropMsg :: StoreType -> Text
    dropMsg Fur    = "scraps of fur."
    dropMsg Meat   = "bits of meat."
    dropMsg Scales = "strange scales."
    dropMsg Teeth  = "scattered teeth."
    dropMsg Cloth  = "tattered cloth."
    dropMsg Charm  = "a crudely made charm."
    dropMsg _      = error "Invalid dropMsg type"

collectIncome :: GameAction
collectIncome state =
  let foldFun s worker wdata =
        -- TODO: Implement thieves
        let s' = M.unionWith (+) s (getIncomeStores worker wdata)
        in
          if wdata^.ticksLeft > 0 || any (<0) (M.elems s')
          then s
          else s'
  in return $ state & workers .~ ws
                    & stores  %~ (\s -> M.foldlWithKey foldFun s ws)
  where
    ws = (state^.workers) & traverse . ticksLeft %~ (\t -> if t <= 0 then incomeTicks else t-1)

increasePopulation :: GameAction
increasePopulation state
  | space > 0 = do
      let
        Just rng = state^.randomGens . at RNGWanderer

        (p',g) = explodingDie [8,8, 5,5,5, 3,3] rng
        p      = min p' space
        msg | p == 1    = "a stranger arrives in the night."
            | p < 5     = "a weathered family takes up in one of the huts."
            | p < 10    = "a small group arrives, all dust and bones."
            | p < 30    = "a convoy lurches in, equal parts worry and hope."
            | otherwise = "the town's booming. word does get around."

      tell [msg]
      return $ state & population                  +~ p
                     & randomGens . at RNGWanderer ?~ g

  | otherwise = return state

  where
    capacity = (state^.buildings . at Hut . non 0) * hutCapacity
    space    = capacity - state^.population
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
  case maybeState of
    Just s  -> return s
    Nothing -> do
      -- Create a RNG for every RandomGensType type.
      rngs <- fmap M.fromList $ mapM newRngPair [toEnum 0 ..]
      return $ def & randomGens .~ rngs

  where
    newRngPair k = do
      rng <- liftIO newStdGen
      return (k, rng)

--------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = mainWidgetWithCss css body
  where css = $(embedFile "css/style.css")

body :: MonadWidget t m => m ()
body = do
  evPostBuild <- getPostBuild

  evInitState <- performEvent $ ffor evPostBuild $ const $ do
    st <- liftJSM loadGameState
    return $ \_ -> return st

  t0 <- liftIO getCurrentTime
  animationTick'   <- tickLossy animationTickDt t0
  evAdjustRoomTemp <- tickLossy adjustRoomTempDelay t0
  evCollectIncome  <- tickLossy 1 t0

  rng <- liftIO newStdGen
  evIncreasePopulation <- poissonLossy rng incrPopulationRate t0


  elClass "div" "wrapper" $ do
    rec
      (isSelectedMap, evUserActions) <- runGameT (GameConfig animationTick' dynState) $
        tabDisplayDyn "header" "headerButton" "selected" "main" $ M.fromList
          [ (Room,    ( describeRoom <$> dynFireLevel
                      , constDyn True
                      , roomTab) )
          , (Village, ( dynState <&> (\s -> describeVillage $ s ^. buildings)
                      , (Village `S.member`) <$> dynAllowedLocations
                      , villageTab) )
          ]

      (dynState, dynActionMsgs) <- fmap splitDynPure $ foldDyn foldFun (def, []) $ mergeWith (>=>)
        [ (foldl1' (>=>)) <$> evUserActions
        , coolFire <$ evCoolFire
        , adjustRoomTemp <$ evAdjustRoomTemp
        , adjustBuilderLevel <$ evAdjustBuilder
        , unlockVillage <$ evUnlockVillage
        -- NOTE: using updated dynCurLocation directly causes infinite loop
        , arriveVillage <$ ffilter id (updated $ isSelectedMap ! Village)

        , collectIncome <$ evCollectIncome

        , increasePopulation <$ evIncreasePopulation

        , evInitState
        ]
      let evActionMsgs = ffilter (not . null) $ updated dynActionMsgs

      dynAllowedLocations <- dynField dynState allowedLocations

      dynBuilderLevel <- dynField dynState builderLevel
      evAdjustBuilder <- fmap (gate $ current $ dynBuilderLevel <&> (< BuilderWorking)) $
        tickLossyFrom adjustBuilderDelay t0 (updated dynStokedForFirstTime)

      evUnlockVillage <- delay unlockVillageDelay $ ffilter (== BuilderCollapsed) $ updated dynBuilderLevel

      -- Fires whenever firelevel updates, _or_ whenever fire is stoked.
      dynFireLevel <- fmap (fmap fst) . holdUniqDyn $ dynState <&> (\s -> (s^.fireLevel, s^.timesFireStoked))
      dynStokedForFirstTime <- holdUniqDyn $ dynState <&> (\s -> (s^.timesFireStoked) > 0)
      evCoolFire <- debounce coolFireDelay $ leftmost [updated dynFireLevel, evCoolFire]

    dynRoomTemp  <- dynField dynState roomTemp

    notificationsPane $ mergeWith (++)
      [ evActionMsgs

      , notifyDyn dynFireLevel

      , notifyDyn dynRoomTemp
      , notifyDyn dynBuilderLevel
      ]

    elClass "div" "menu" $ do
      evSave <- menuButton "save."
      performEvent_ $ ffor (tag (current dynState) evSave) $ liftJSM . saveGameState

  where
#ifdef DEBUG
    coolFireDelay       = 60
    adjustRoomTempDelay = 5
    adjustBuilderDelay  = 5
    unlockVillageDelay  = 5
    incrPopulationRate  = 1/10   -- In Hz
#else
    coolFireDelay       = 5 * 60
    adjustRoomTempDelay = 30
    adjustBuilderDelay  = 30
    unlockVillageDelay  = 15
    incrPopulationRate  = 1/75   -- In Hz
#endif

    foldFun :: (GameState -> Writer [Text] GameState) -> (GameState, [Text]) -> (GameState, [Text])
    foldFun f (st, _) = runWriter $ f st

    dynField :: (MonadWidget t m, Eq a) => Dynamic t GameState -> Getting a GameState a -> m (Dynamic t a)
    dynField dynState field = holdUniqDyn $ dynState <&> (^.field)

    menuButton :: MonadWidget t m => Text -> m (Event t ())
    menuButton lbl = do
      (e,_) <- elClass' "span" "menuButton" $ text lbl
      return $ () <$ domEvent Click e

roomTab :: forall t m. MonadGame t m => m ()
roomTab = elClass "div" "location" $ do
  dynFireLevel <- holdUniqDyn =<< asksGameState fireLevel
  dynMaybeWood <- holdUniqDyn =<< asksGameState (stores . at Wood)

  evStokeFire <-
    button $ def & label        .~ fmap fireBtnLabel dynFireLevel
                 & cooldownSecs .~ fmap stokeCooldown dynMaybeWood
  performAction evStokeFire stokeFire

  dynBuilderLevel <- holdUniqDyn =<< (fmap (fmap (^.builderLevel)) $ asks (^.gameState))
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

    craftButton $ def & buildingType   .~ Hut
                      & maxCraftAmount .~ 20
                      & availableMsg   ?~ "builder says there are more wanderers. says they'll work, too."
                      & buildMsg       ?~ "builder puts up a hut, out in the forest. says word will get around."
                      & maxMsg         ?~ "no more room for huts."
                      & craftCost      .~ (\b -> Wood =: (((b ^. at Hut . non 0) + 2) * 50))

    craftButton $ def & buildingType   .~ Lodge
                      & maxCraftAmount .~ 1
                      & availableMsg   ?~ "villagers could help hunt, given the means."
                      & buildMsg       ?~ "the hunting lodge stands in the forest, a ways out of town."
                      & craftCost      .~ const [(Wood, 200), (Fur, 10), (Meat, 5)]

    craftButton $ def & buildingType   .~ TradingPost
                      & maxCraftAmount .~ 1
                      & availableMsg   ?~ "a trading post would make commerce easier."
                      & buildMsg       ?~ "now the nomads have a place to set up shop, they might stick around a while."
                      & craftCost      .~ const [(Wood, 400), (Fur, 100)]

    craftButton $ def & buildingType   .~ Tannery
                      & maxCraftAmount .~ 1
                      & availableMsg   ?~ "builder says leather could be useful. says the villagers could make it."
                      & buildMsg       ?~ "tannery goes up quick, on the edge of the village."
                      & craftCost      .~ const [(Wood, 500), (Fur, 50)]

    craftButton $ def & buildingType   .~ Smokehouse
                      & maxCraftAmount .~ 1
                      & availableMsg   ?~ "should cure the meat, or it'll spoil. builder says she can fix something up."
                      & buildMsg       ?~ "builder finishes the smokehouse. she looks hungry."
                      & craftCost      .~ const [(Wood, 600), (Meat, 50)]

    craftButton $ def & buildingType   .~ Workshop
                      & maxCraftAmount .~ 1
                      & availableMsg   ?~ "builder says she could make finer things, if she had the tools."
                      & buildMsg       ?~ "workshop's finally ready. builder's excited to get to it."
                      & craftCost      .~ const [(Wood, 800), (Leather, 100), (Scales, 10)]

    craftButton $ def & buildingType   .~ Steelworks
                      & maxCraftAmount .~ 1
                      & availableMsg   ?~ "builder says villagers could make steel, given the tools."
                      & buildMsg       ?~ "a haze falls over the village as the steelworks fires up."
                      & craftCost      .~ const [(Wood, 1500), (Iron, 100), (Coal, 100)]

    craftButton $ def & buildingType   .~ Armoury
                      & maxCraftAmount .~ 1
                      & availableMsg   ?~ "builder says it'd be useful to have a steady source of bullets."
                      & buildMsg       ?~ "armoury's done, welcoming back the weapons of the past."
                      & craftCost      .~ const [(Wood, 1500), (Steel, 100), (Sulphur, 50)]

  buttonCol (constDyn False)  $ do
    el "h1" $ text "craft:"

    button $ def & label        .~ constDyn "torch"
                 & cooldownSecs .~ constDyn 3 -- TEST

  buttonCol (constDyn False) $ do
    el "h1" $ text "buy:"

    button $ def & label        .~ constDyn "scales"
                 & cooldownSecs .~ constDyn 3 -- TEST

  dynAllowedLocations <- holdUniqDyn =<< asksGameState allowedLocations

  dynStores <- holdUniqDyn =<< asksGameState stores
  dynWorkers <- holdUniqDyn =<< asksGameState workers
  let dynStoresField = zipDynWith makeStoresField dynStores dynWorkers

  dynWidget ((Village `S.member`) <$> dynAllowedLocations) $ elClass "div" "storesCol" $
    storesFieldset "stores" (constDyn "stores") Nothing dynStoresField

  return ()

  where
    stokeCooldown Nothing  = stokeCooldown'
    stokeCooldown (Just w) | w > 0     = stokeCooldown'
                           | otherwise = 0
#ifdef DEBUG
    stokeCooldown' = 1
#else
    stokeCooldown' = 10
#endif

    buttonCol isVisible w = dynWidget isVisible $ elClass "div" "roomButtonCol" w >> return ()

    fireBtnLabel fl
      | fl == minBound = "light fire"
      | otherwise      = "stoke fire"

villageTab :: forall t m. MonadGame t m => m ()
villageTab = elClass "div" "location" $ do
  dynBuildings  <- holdUniqDyn =<< asksGameState buildings
  dynPopulation <- holdUniqDyn =<< asksGameState population
  let dynHuts  = dynBuildings <&> (^. at Hut . non 0)

  evGatherWood <-
    button $ def & label        .~ constDyn "gather wood"
                 & cooldownSecs .~ constDyn gatherWoodCooldown
  performAction evGatherWood gatherWood

  evCheckTraps <-
    button $ def & label        .~ constDyn "check traps"
                 & cooldownSecs .~ constDyn checkTrapsCooldown
                 & visible      .~ (dynBuildings <&> (has $ ix Trap))
                 & enabled      .~ (dynBuildings <&> (\b -> b ^. at Trap . non 0 > 0))
  performAction evCheckTraps checkTraps


  dynStores <- holdUniqDyn =<< asksGameState stores
  dynWorkers <- holdUniqDyn =<< asksGameState workers

  let dynStoresField = zipDynWith makeStoresField dynStores dynWorkers
  let dynBuildingsField = dynBuildings <&> (traverse %~ (\v -> (v,[])))

  elClass "div" "storesCol" $ do
    storesFieldset "stores" (buildingsLegend <$> dynHuts) (Just $ zipDynWith populationLegend dynHuts dynPopulation) dynBuildingsField
    storesFieldset "stores" (constDyn "stores") Nothing dynStoresField
  return ()

  where
#ifdef DEBUG
    gatherWoodCooldown = 1
    checkTrapsCooldown = 1
#else
    gatherWoodCooldown = 60
    checkTrapsCooldown = 90
#endif

    buildingsLegend h
      | h <= 0    = "forest"
      | h == 1    = "hut"
      | otherwise = "village"

    populationLegend h p = "pop " <> showt p <> "/" <> showt (h * hutCapacity)

makeStoresField :: Stores -> Workers -> M.Map StoreType (Int, [TooltipRow])
makeStoresField ss ws = M.mapWithKey (\k v -> (v, toTooltip $ workerRates ^. at k . non [])) ss
  where
    workerRates = getStoreWorkerRates ws

    toTooltip rates =
      [def & keyText   .~ showRowKey k
           & valueText .~ showValue v
        | (k,v) <- M.toList rates]
       ++
      [def & keyText   .~ "total"
           & valueText .~ showValue (M.foldl' (+) 0 rates)
           & classes   .~ "total"]

    showValue v = monoidGuard (v>0) "+" <> showt v <> " per " <> showt incomeTicks <> "s"
