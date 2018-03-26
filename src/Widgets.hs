{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Widgets where

import           Control.Lens
import           Control.Monad              (void, when)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Writer.Class
import           Data.Default
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust, fromJust, listToMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Set                   as S
import           Data.Time.Clock
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Reflex.Dom                 hiding (button)
import           TextShow

import           State
import           Utils

-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <div> with one <div> per child; clicking a <div> displays
--   the corresponding child and hides all others.
--
--   This variant allows dynamic visibility of the tabs.
tabDisplayDyn :: forall t m k. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
  => Text               -- ^ Class applied to the tab itself
  -> Text               -- ^ Class applied to tab button elements
  -> Text               -- ^ Class applied to currently selected tab button element
  -> Text               -- ^ Class applied to child div
  -> (M.Map k (Dynamic t Text, Dynamic t Bool, m ())) -- ^ Map from (arbitrary) key to (tab label, isVisible, child widget)
  -> m (M.Map k (Dynamic t Bool)) -- ^ Map from key to Dynamic isSelected
tabDisplayDyn tabClass tabButtonClass selectedClass childClass tabItems = do
  let t0 = listToMaybe $ M.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- elAttr "div" ("class" =: tabClass) $ do
        tabClicksList :: [Event t k] <- M.elems <$> imapM (\k (s,a,_) -> headerBarLink s k a $ demuxed currentTab (Just k)) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        fmap demux $ holdDyn t0 $ fmap Just eTabClicks

      isSelectedMap <- elClass "div" childClass $
        iforM tabItems $ \k (_,_,w) -> do
          let isSelected = demuxed currentTab $ Just k
              attrs = ffor isSelected $ \s -> monoidGuard (not s) ("style" =: "display:none;")
          elDynAttr "div" attrs w
          return isSelected

  return isSelectedMap

  where
    headerBarLink :: Dynamic t Text -> k -> Dynamic t Bool -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k isVisible isSelected = do
      let attrs = zipDynWith (\v s -> monoidGuard (not v) ("style" =: "display:none;")
                                   <> ("class" =: (tabButtonClass <> monoidGuard s (" " <> selectedClass))))
                           isVisible isSelected
      (l,_) <- elDynAttr' "div" attrs $ dynText x
      return $ k <$ domEvent Click l

-- | A text notifications pane.
notificationsPane ::(MonadWidget t m) => Event t [Text] -> m ()
notificationsPane notification = do
  notifications <- foldDyn (\n ns -> take maxNotifications . filter (not . T.null) $ n ++ ns) [] notification
  elClass "div" "notifications" $ do
    simpleList notifications $ elClass "div" "notification" . dynText
    elClass "div" "notifyGradient" blank
  return ()

  where maxNotifications = 32

{-# INLINABLE dynWidget #-}
dynWidget :: (MonadWidget t m) => Dynamic t Bool -> m () -> m (Event t ())
dynWidget isVisible widget = dyn $ w <$> isVisible
  where w False = return ()
        w True  = widget

{-# INLINABLE dynWidget' #-}
dynWidget' :: (MonadWidget t m) => Dynamic t Bool -> m (Event t a) -> m (Event t a)
dynWidget' isVisible widget = do
  evW <- dyn $ w <$> isVisible
  return $ coincidence evW

  where w False = return never
        w True  = widget

data TooltipRow = TooltipRow
 { _tooltipRowKeyText     :: !Text
 , _tooltipRowValueText   :: !Text
 , _tooltipRowClasses     :: !Text
 } deriving (Eq, Show)

instance Default TooltipRow where
  def = TooltipRow "" "" ""

makeFields ''TooltipRow

tooltip :: MonadWidget t m => Dynamic t [TooltipRow] -> m ()
tooltip rows =
  void $ dynWidget ((not . null) <$> rows) $
    elClass "div" "tooltip bottom right" $ do
      simpleList rows $ \dynRow -> do
        let cl = dynRow <&> (^.classes)
        elDynClass "div" (("rowkey " <>) <$> cl) $ dynText $ dynRow <&> (^.keyText)
        elDynClass "div" (("rowval " <>) <$> cl) $ dynText $ dynRow <&> (^.valueText)
      return ()


data ButtonConfig t = ButtonConfig
  { _buttonConfigLabel        :: !(Dynamic t Text)
  , _buttonConfigCooldownSecs :: !(Dynamic t Float)
  , _buttonConfigVisible      :: !(Dynamic t Bool)
  , _buttonConfigEnabled      :: !(Dynamic t Bool)
  , _buttonConfigTooltipRows  :: !(Dynamic t [TooltipRow])
  }

makeFields ''ButtonConfig


instance Reflex t => Default (ButtonConfig t) where
  {-# INLINABLE def #-}
  def = ButtonConfig "button" (constDyn 0) (constDyn True) (constDyn True) (constDyn [])

-- | A div button with a cooldown bar.
button :: (MonadWidget t m, MonadReader (GameConfig t) m) => ButtonConfig t -> m (Event t ())
button cfg = do
  evTick <- asks (^.animationTick)
  -- TODO: better initial value for tLastClick
  t0 <- ((-1000) `addUTCTime`) <$> liftIO getCurrentTime
  rec
    -- Behavior t Float : UTC time of last click event
    tLastClick <- hold t0 =<< (performEvent . ffor evClick $ const $ liftIO getCurrentTime)
    -- Dynamic t Float : (approximate) time passed since click
    dtSinceLastClick <- holdDyn ((1/0) :: Float) $
                          attachWith (\tl tickInfo -> realToFrac $ (_tickInfo_lastUTC tickInfo) `diffUTCTime` tl)
                                     tLastClick evTick

    let curCooldown = zipDynWith (\cs dt -> max 0 (cs-dt)) (cfg^.cooldownSecs) dtSinceLastClick
        clicksAllowed = zipDynWith (\e c -> e && c <= 0) (cfg^.enabled) curCooldown
        attrs = zipDynWith (\v e -> monoidGuard (not v) ("style" =: "display:none;")
                                      <> ("class" =: (   "button" <> monoidGuard (not e) (" disabled"))))
                             (cfg^.visible) clicksAllowed

    (e,_) <- elDynAttr' "div" attrs $ do
      dynText $ cfg^.label

      -- Cooldown bar
      let cooldownAttrs = zipDynWith (\cur cs -> ("class" =: "cooldown")
                                                 <> monoidGuard (cur>0)
                                                                ("style" =: (
                                                                   "width: " <> showt (cur*100/cs) <> "%; overflow: hidden;")))
                                     curCooldown (cfg^.cooldownSecs)

      elDynAttr "div" cooldownAttrs blank

      tooltip $ cfg^.tooltipRows

    let evClick = gate (current clicksAllowed) $ domEvent Click e
  return evClick


data CraftButtonConfig = CraftButtonConfig
  { _craftButtonConfigBuildingType   :: BuildingType
  , _craftButtonConfigMaxCraftAmount :: Int
  , _craftButtonConfigAvailableMsg   :: Maybe Text
  , _craftButtonConfigBuildMsg       :: Maybe Text
  , _craftButtonConfigMaxMsg         :: Maybe Text
  , _craftButtonConfigCraftCost      :: Buildings -> Stores
  }

makeFields ''CraftButtonConfig

instance Default CraftButtonConfig where
  {-# INLINABLE def #-}
  def = CraftButtonConfig undefined 0 Nothing Nothing Nothing undefined

craftButton :: MonadGame t m => CraftButtonConfig -> m ()
craftButton cfg = do
  dynState     <- asks (^.gameState)
  dynBuildings <- asksGameState buildings
  dynStores    <- asksGameState stores

  let dynCost' = cfg^.craftCost <$> dynBuildings
  dynCost <- holdUniqDyn dynCost'

  let numCrafted = dynBuildings <&> (^. at bdgType . non 0)

  lessThanMax <- holdUniqDyn $ numCrafted <&> (< cfg^.maxCraftAmount)

  evPostBuild <- delay 0 =<< getPostBuild

  let isAvailable' = isAvail <$> dynState
      evAvailable' = leftmost
        [ attachWithMaybe (\old new -> if new && not old then Just True else Nothing)
                          (current isAvailable') (updated isAvailable')
        , ffilter id $ current isAvailable' <@ evPostBuild
        ]

  isAvailable <- holdDyn False evAvailable'

  evAvailable <- performEvent $ ffor evAvailable' $ const $ return ()

  performAction evAvailable $ \st ->
    if st^.buildingsAvailable & has (ix bdgType)
      then return st
      else do
        when (isJust (cfg^.availableMsg)) $
          let Just msg = cfg^.availableMsg in tell [msg]
        return $ st & buildingsAvailable %~ S.insert bdgType


  evBuild <- button $ def & label       .~ constDyn (toSpaceCase $ showt bdgType)
                          & visible     .~ isAvailable
                          & enabled     .~ lessThanMax
                          & tooltipRows .~ fmap (\c -> [def & keyText   .~ showRowKey k
                                                            & valueText .~ showt v
                                                        | (k,v) <- M.toList c]) dynCost

  performAction evBuild $ \st -> do
    -- Craft the building if there are enough materials
    let cost = (cfg^.craftCost) (st^.buildings)
        ss   = st^.stores

    if st^.roomTemp <= Cold
      then do
        tell ["builder just shivers."]
        return st
    else if cost .<=. ss
      then do
        when (isJust (cfg^.maxMsg) && (st^.buildings . at bdgType . non 0) == cfg^.maxCraftAmount - 1) $
          let Just msg = cfg^.maxMsg in tell [msg]
        when (isJust (cfg^.buildMsg)) $
          let Just msg = cfg^.buildMsg in tell [msg]

        return $ st & stores    .~ M.differenceWith (\a b -> Just (a-b)) ss cost
                    & buildings . at bdgType . non 0 +~ 1
    else do
        tell ["not enough materials."]
        return st
  -- performAction uses tellEvent from EventWriter.

  where
    bdgType = cfg^.buildingType

    -- Show button if one has already been built, or
    -- we have >= 1/2 * Wood, and all other components have been seen.
    isAvail :: GameState -> Bool
    isAvail st =  (st^.buildingsAvailable & has (ix bdgType))
               || ((s ^. at Wood . non 0) * 2 >= (c ^. at Wood . non 0)
                   && M.isSubmapOfBy (\_ _ -> True) c s)
      where s = st^.stores
            b = st^.buildings
            c = (cfg^.craftCost) b

storesFieldset :: (MonadWidget t m, Ord k, TextShow k, TextShow v)
               => Text -> Dynamic t Text -> Maybe (Dynamic t Text) -> Dynamic t (M.Map k (v, [TooltipRow])) -> m ()
storesFieldset className legend maybeLegend2 itemsMap = elClass "fieldset" className $ do
  el "legend" $ dynText legend
  when (isJust maybeLegend2) $
    elClass "span" "legend2" $ dynText (fromJust maybeLegend2)
  listWithKey itemsMap $ \k dynV' -> do
    let (dynV, dynTooltipRows) = splitDynPure dynV'
    elClass "div" "storeRow" $ do
      elClass "div" "rowkey" $ text (showRowKey k)
      elClass "div" "rowval" $ dynText $ showt <$> dynV
      tooltip dynTooltipRows
  return ()
