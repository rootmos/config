module Utils where

import Control.Monad ( liftM, forM )
import Data.Maybe ( catMaybes )
import Data.Bits ( testBit )
import Data.List ( sortBy, find )
import Graphics.X11 ( openDisplay
                    , rootWindow
                    , defaultScreen
                    )
import Graphics.X11.Xrandr
import qualified Graphics.X11.Xlib.Types as Xlib
import qualified Graphics.X11.Types as X
import qualified Data.Map.Strict as M

newtype OutputId = OutputId X.RROutput
  deriving ( Show, Eq, Ord )

newtype MonitorId = MonitorId X.RRCrtc
  deriving ( Show, Eq, Ord )

data Monitor = Monitor { monitorId :: MonitorId
                       , monitorX :: Int
                       , monitorY :: Int
                       , monitorWidth :: Int
                       , monitorHeight :: Int
                       , monitorMode :: Maybe Mode
                       , monitorOutputs :: [OutputId]
                       , monitorChanged :: Bool
                       }
                       deriving (Show)

data Output = Output { outputId :: OutputId
                     , outputName :: String
                     , outputModes :: [Mode]
                     , outputMonitor :: Maybe MonitorId
                     , outputMonitors :: [MonitorId]
                     , outputWidthInMillimeters :: Int
                     , outputHeightInMillimeters :: Int
                     }
                     deriving Show

isOutputConnected :: Output -> Bool
isOutputConnected Output { outputModes = [] } = False
isOutputConnected _ = True

isOutputEnabled :: Output -> Bool
isOutputEnabled Output { outputMonitor = Nothing } = False
isOutputEnabled _ = True

data Setup = Setup { setupOutputs :: M.Map OutputId Output
                   , setupMonitors :: M.Map MonitorId Monitor
                   }
                   deriving Show

newtype ModeId = ModeId X.RRMode
    deriving ( Show, Eq, Ord)

data Mode = Mode { modeId :: ModeId
                 , modeWidth :: Int
                 , modeHeight :: Int
                 , modeHz :: Double
                 , modeInterlaced :: Bool
                 , modeDoubleScan :: Bool
                 }
                 deriving ( Eq, Show )

modeMap :: XRRScreenResources -> M.Map ModeId Mode
modeMap XRRScreenResources { xrr_sr_modes = modes } =
  M.fromAscList $ map modeMaker modes
    where
      modeMaker mi =
        (ModeId (xrr_mi_id mi), Mode { modeId = ModeId (xrr_mi_id mi)
                                     , modeWidth = fromIntegral $ xrr_mi_width mi
                                     , modeHeight = fromIntegral $ xrr_mi_height mi
                                     , modeHz = refreshRate mi
                                     , modeInterlaced = isInterlaced mi
                                     , modeDoubleScan = isDoubleScan mi
                                     })

refreshRate :: XRRModeInfo -> Double
refreshRate mi = dotClock / (hTotal * vTotal)
  where
    dotClock = fromIntegral $ xrr_mi_dotClock mi
    hTotal = fromIntegral $ xrr_mi_hTotal mi
    vTotal
      | (isDoubleScan mi) && not (isInterlaced mi) = 2 * (fromIntegral $ xrr_mi_vTotal mi)
      | not (isDoubleScan mi) && (isInterlaced mi) = (fromIntegral $ xrr_mi_vTotal mi) / 2
      | otherwise = fromIntegral $ xrr_mi_vTotal mi

isInterlaced :: XRRModeInfo -> Bool
isInterlaced mi = testBit (xrr_mi_modeFlags mi) 4

isDoubleScan :: XRRModeInfo -> Bool
isDoubleScan mi = testBit (xrr_mi_modeFlags mi) 5

fetchSetup :: Xlib.Display -> XRRScreenResources -> IO Setup
fetchSetup display res = do
  rawOutputs <- rawOutputsM
  let outputs = M.mapWithKey outputMaker rawOutputs

  rawMonitors <- rawMonitorsM
  let monitors = M.mapWithKey monitorMaker rawMonitors

  return $ Setup { setupOutputs = outputs, setupMonitors = monitors }
    where
      rawOutputsM = liftM makeMapFromMaybes $ forM (xrr_sr_outputs res) (\oid -> do
        maybeOi <- xrrGetOutputInfo display res oid
        return $ ((,) (OutputId oid)) <$> maybeOi)

      rawMonitorsM = liftM makeMapFromMaybes $ forM (xrr_sr_crtcs res) (\cid -> do
        maybeCi <- xrrGetCrtcInfo display res cid
        return $ ((,) (MonitorId cid)) <$> maybeCi)

      makeMapFromMaybes :: Ord k => [Maybe (k, v)] -> M.Map k v
      makeMapFromMaybes = M.fromAscList . sortBy (\x y -> fst x `compare` fst y) . catMaybes

      outputMaker oid oi =
        Output { outputId = oid
               , outputName = (xrr_oi_name oi)
               , outputModes = catMaybes $ map (\m -> M.lookup (ModeId m) (modeMap res)) (xrr_oi_modes oi)
               , outputMonitor = case (fromIntegral $ xrr_oi_crtc oi) of
                                   0 -> Nothing
                                   i -> Just (MonitorId i)
               , outputMonitors = map (MonitorId . fromIntegral) (xrr_oi_crtcs oi)
               , outputWidthInMillimeters = fromIntegral $ xrr_oi_mm_width oi
               , outputHeightInMillimeters = fromIntegral $ xrr_oi_mm_height oi
               }

      monitorMaker mid ci =
        Monitor { monitorId = mid
                , monitorX = (fromIntegral $ xrr_ci_x ci)
                , monitorY = (fromIntegral $ xrr_ci_y ci)
                , monitorWidth = (fromIntegral $ xrr_ci_width ci)
                , monitorHeight = (fromIntegral $ xrr_ci_height ci)
                , monitorMode = M.lookup (ModeId $ xrr_ci_mode ci) (modeMap res)
                , monitorOutputs = map (OutputId . fromIntegral) (xrr_ci_outputs ci)
                , monitorChanged = False
                }

currentDefaultDisplay :: IO (Xlib.Display, XRRScreenResources)
currentDefaultDisplay = do
  display <- openDisplay ""
  root <- rootWindow display (defaultScreen display)
  Just res <- xrrGetScreenResourcesCurrent display root
  return (display, res)

lookupMonitor :: MonitorId -> Setup -> Maybe Monitor
lookupMonitor i = M.lookup i . setupMonitors

lookupOutput :: OutputId -> Setup -> Maybe Output
lookupOutput i = M.lookup i . setupOutputs

findOutput :: String -> Setup -> Maybe Output
findOutput name = find ((== name) . outputName) . setupOutputs
