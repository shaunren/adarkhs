{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE StandaloneDeriving     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module State where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Writer.Stricter
import           Data.Aeson
import           Data.Default
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import           Data.Text            (Text, pack, unpack)
import           GHC.Generics
import           Reflex
import           Reflex.Dom           (MonadWidget)
import           System.Random        (StdGen)
import           TextShow.TH

class NotifyShow a where
  showN :: a -> Text

type CooldownMap = M.Map Text Float

data Location = Room | Village | World | Spaceship
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, ToJSON, FromJSON)

data FireLevel = FireDead | FireSmoldering | FireFlickering | FireBurning | FireRoaring
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, ToJSON, FromJSON)

instance NotifyShow FireLevel where
  {-# INLINABLE showN #-}
  showN FireDead = "the fire is dead."
  showN FireSmoldering = "the fire is smoldering."
  showN FireFlickering = "the fire is flickering."
  showN FireBurning = "the fire is burning."
  showN FireRoaring = "the fire is roaring."

data RoomTempLevel = Freezing | Cold | Mild | Warm | Hot
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, ToJSON, FromJSON)

instance NotifyShow RoomTempLevel where
  {-# INLINABLE showN #-}
  showN Freezing  = "the room is freezing."
  showN Cold = "the room is cold."
  showN Mild = "the room is mild."
  showN Warm = "the room is warm."
  showN Hot = "the room is hot."

data BuilderLevel = BuilderDNE | BuilderApproaching | BuilderCollapsed | BuilderShivering | BuilderSleeping | BuilderWorking
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, ToJSON, FromJSON)

instance NotifyShow BuilderLevel where
  {-# INLINABLE showN #-}
  showN BuilderDNE = ""
  showN BuilderApproaching = ""
  showN BuilderCollapsed = "a ragged stranger stumbles through the door and collapses in the corner."
  showN BuilderShivering = "the stranger shivers, and mumbles quietly. her words are unintelligible."
  showN BuilderSleeping  = "the stranger in the corner stops shivering. her breathing calms."
  showN BuilderWorking   = "the stranger is standing by the fire. she says she can help. says she builds things."

data StoreType = Wood | Fur | Meat | Bait | CuredMeat | Teeth | Cloth | Scales | Leather | Iron | Coal | Steel | Sulphur | Charm | Bullets
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

$(deriveTextShow ''StoreType)


data BuildingType = Hut | Cart | Trap | IronMine | CoalMine | SulphurMine | Lodge | TradingPost | Tannery | Smokehouse | Workshop | Steelworks | Armoury
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

$(deriveTextShow ''BuildingType)

type Stores = M.Map StoreType Int
type Buildings = M.Map BuildingType Int

data WorkerType = Builder | Gatherer | Hunter | Trapper | Tanner | Charcutier | IronMiner | CoalMiner | SulphurMiner | Steelworker | Armourer
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

$(deriveTextShow ''WorkerType)

data WorkerData = WorkerData
  { _workerDataNumWorkers :: !Int
  , _workerDataTicksLeft  :: !Int
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

makeFields ''WorkerData

instance Default WorkerData where
  {-# INLINE def #-}
  def = WorkerData 0 0

workerIncomeStores :: M.Map WorkerType Stores
workerIncomeStores = [ (Builder,  [(Wood, 2)])
                     , (Gatherer, [(Wood, 1)])
                     , (Hunter, [(Fur, 1), (Meat, 1)])
                     , (Trapper, [(Meat, -1), (Bait, 1)])
                     , (Tanner, [(Fur, -5), (Leather, 1)])
                     , (Charcutier, [(Meat, -5), (Wood, -5), (CuredMeat, 1)])
                     , (IronMiner, [(CuredMeat, -1), (Iron, 1)])
                     , (CoalMiner, [(CuredMeat, -1), (Coal, 1)])
                     , (SulphurMiner, [(CuredMeat, -1), (Sulphur, 1)])
                     , (Steelworker, [(Iron, -1), (Coal, -1), (Steel, 1)])
                     , (Armourer, [(Steel, -1), (Sulphur, -1), (Bullets, 1)])
                     ]

type Workers = M.Map WorkerType WorkerData

{-# INLINE getIncomeStores #-}
getIncomeStores :: WorkerType -> WorkerData -> Stores
getIncomeStores w d
  | n < 1     = []
  | otherwise = (workerIncomeStores ^. at w . _Just) & traverse *~ n
  where n = d^.numWorkers

{-# INLINE getStoreWorkerRates #-}
getStoreWorkerRates :: Workers -> M.Map StoreType (M.Map WorkerType Int)
getStoreWorkerRates ws =
  M.foldlWithKey
    (\m worker wdata -> M.unionWith (M.unionWith (+)) m . M.map (M.singleton worker) $ getIncomeStores worker wdata)
    [] ws

data RandomGenType = RNGTrap | RNGWanderer | RNGWorld
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance Eq StdGen where
  g == g' = show g == show g'

instance ToJSON StdGen where
  {-# INLINE toJSON #-}
  toJSON = String . pack . show

instance FromJSON StdGen where
  {-# INLINE parseJSON #-}
  parseJSON = withText "Text" $ return . read . unpack

type RandomGens = M.Map RandomGenType StdGen


data GameState = GameState
  { _gameStateCurrentLocation  :: !Location
  , _gameStateAllowedLocations :: !(S.Set Location)
  , _gameStateSeenVillage      :: !Bool
  , _gameStateFireLevel        :: !FireLevel
  , _gameStateTimesFireStoked  :: !Word
  , _gameStateRoomTemp         :: !RoomTempLevel
  , _gameStateBuilderLevel     :: !BuilderLevel
  , _gameStatePopulation       :: !Int
  , _gameStateCooldown         :: !CooldownMap
  , _gameStateStores           :: !Stores
  , _gameStateBuildings        :: !Buildings
  , _gameStateBuildingsAvailable :: !(S.Set BuildingType)
  , _gameStateWorkers          :: !Workers
  , _gameStateRandomGens       :: !RandomGens
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

makeFields ''GameState

instance Default GameState where
  {-# INLINE def #-}
  def = GameState Room [Room] False FireDead 0 Freezing BuilderDNE 0 [] [] [] [] [] []

data GameConfig t = GameConfig
  { _gameConfigAnimationTick :: !(Event t TickInfo)
  , _gameConfigGameState     :: !(Dynamic t GameState)
  , _gameConfigStores        :: !(Dynamic t Stores)    -- holdUniqDyn
  , _gameConfigBuildings     :: !(Dynamic t Buildings) -- holdUniqDyn
  }

makeFields ''GameConfig

-- | A game action modifies a GameState and/or add notifications.
type GameAction = GameState -> Writer [Text] GameState



type GameT t m a = ReaderT (GameConfig t) (EventWriterT t [GameAction] m) a

type GameEventWriter t m = EventWriter t [GameAction] m
-- | Type constraints for GameT
type MonadGame t m = (MonadWidget t m, MonadReader (GameConfig t) m, GameEventWriter t m)

{-# INLINE runGameT #-}
runGameT :: (Reflex t, Monad m) => GameConfig t -> GameT t m a -> m (a, Event t [GameAction])
runGameT cfg game = runEventWriterT $ runReaderT game cfg

{-# INLINE execGameT #-}
execGameT :: (Reflex t, Monad m) => GameConfig t -> GameT t m a -> m (Event t [GameAction])
execGameT cfg game = fmap snd $ runGameT cfg game

{-# INLINE performAction #-}
performAction :: (Reflex t, GameEventWriter t m) => Event t a  -> GameAction -> m ()
performAction ev action = performAction' ev (const action)

{-# INLINE performAction' #-}
performAction' :: (Reflex t, GameEventWriter t m) => Event t a -> (a -> GameAction) -> m ()
performAction' ev fAction = tellEvent $ fmap (\x -> [fAction x]) ev

-- | Retrieves gameState.field from the Reader environment.
{-# INLINE asksGameState #-}
asksGameState :: (Reflex t, MonadReader (GameConfig t) m) => Getting a GameState a -> m (Dynamic t a)
asksGameState field  = fmap (fmap (^.field)) $ asks (^.gameState)
