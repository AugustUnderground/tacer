{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ACEConfig (..)
    , ACEIncludes (..)
    , ACEParameters (..)
    , ACEConstraints (..)
    , ACEInstance (..)
    , ACEdcop (..)
    , ACEdcmatch (..)
    , parseACEConfig
    , fillPropertiesTemplate
    , fillTestbenchTemplate
    , fillDutTemplate
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Data.Char
import Data.Maybe
import GHC.Generics
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data ACEIncludes = ACEIncludes { includeFile    :: T.Text
                               , includeSection :: T.Text
                               } deriving (Show, Generic)

data ACEParameters = ACEParameters { cl   :: Float
                                   , rl   :: Float
                                   , i0   :: Float
                                   , vs   :: Float
                                   , vsup :: Float
                                   , area :: T.Text
                                   } deriving (Show, Generic)

data ACEConstraints = ACEConstraints { minW  :: Float
                                     , minL  :: Float
                                     , maxW  :: Float
                                     , maxL  :: Float
                                     , gridW :: Float
                                     , gridL :: Float
                                     } deriving (Show, Generic)

data ACEInstance = ACEInstance { nmosInstance      :: T.Text
                               , nmosPostFix       :: T.Text
                               , pmosInstance      :: T.Text
                               , pmosPostFix       :: T.Text
                               , capInstance       :: T.Text
                               , resInstance       :: T.Text
                               , derivedInstances  :: [T.Text]
                               , derivedStatements :: [T.Text]
                               } deriving (Show, Generic)

data ACEdcop = ACEdcop { dcopParameters :: [T.Text]
                       , dcopUnits      :: [T.Text]
                       } deriving (Show, Generic)

data ACEdcmatch = ACEdcmatch { nmosDCmParameters :: [T.Text]
                             , pmosDCmParameters :: [T.Text]
                             , unitsDCm          :: [T.Text]
                             } deriving (Show, Generic)

data ACEConfig = ACEConfig { technology        :: T.Text
                           , aceID             :: T.Text
                           , scale             :: Float
                           , includes          :: [ACEIncludes]
                           , parameters        :: ACEParameters
                           , sizingParameters  :: [T.Text]
                           , sizingInit        :: [Float]
                           , sizingConstraints :: ACEConstraints
                           , deviceInstance    :: ACEInstance
                           , dcop              :: ACEdcop
                           , dcmatch           :: ACEdcmatch
                           } deriving (Show, Generic)

instance FromJSON ACEIncludes
instance FromJSON ACEParameters
instance FromJSON ACEConstraints
instance FromJSON ACEInstance
instance FromJSON ACEdcop
instance FromJSON ACEdcmatch
instance FromJSON ACEConfig

parseACEConfig :: BS.ByteString -> ACEConfig
parseACEConfig bs = cfg
    where parsedCfg = Y.decodeEither' bs :: Either Y.ParseException ACEConfig
          cfg = case parsedCfg of (Left e)  -> error (show e)
                                  (Right c) -> c

genDCmatchJson' :: T.Text -> (T.Text, T.Text, T.Text) -> (T.Text, T.Text, T.Text) -> T.Text -> T.Text
genDCmatchJson' ds (ni, pn, un) (pi, pp, up) cs = foldl T.append "" 
        [ "\n        \"" , ds , "/" , i' , "/" , T.toLower cs , "\":{\n"
        , "          \"unit\": \"" , u' , "\",\n" 
        , "          \"reference\": \"DUT." , ds , "." , p' , "(m)." , cs , "\"\n"
        , "        }," ]
    where p' = if T.isPrefixOf "MN" ds then pn else pp 
          u' = if T.isPrefixOf "MN" ds then un else up
          i' = if T.isPrefixOf "MN" ds then ni else pi

cpf :: T.Text -> T.Text -> T.Text
cpf a b = jpf (T.commonPrefixes a b)
    where jpf (Just (a,_,_))  = a
          jpf Nothing = b

dcmShort' :: Maybe T.Text -> T.Text
dcmShort' (Just txt) = txt
dcmShort' Nothing = ""

dcmShort :: [T.Text] -> T.Text -> T.Text
dcmShort ps p = dcmShort' $ T.stripPrefix cp =<< T.stripSuffix cs p
    where cp = foldl cpf "" ps
          cs = T.reverse . foldl cpf "" . map T.reverse $ ps

genDCmatchJson :: ACEConfig -> T.Text
genDCmatchJson cfg = T.init . T.concat 
                   $ [ genDCmatchJson' ds pn pp cs 
                     | ds <- dutInstances (aceID cfg)
                     , pn <- zip3 nss nps dcu
                     , pp <- zip3 pss pps dcu
                     , cs <- ["Contribution", "Sensitivity"] ]
    where dcm = dcmatch cfg
          nps = nmosDCmParameters dcm
          nss = map (dcmShort nps) nps
          pps = pmosDCmParameters dcm
          pss = map (dcmShort pps) pps
          dcu = unitsDCm dcm

genDCopJson' :: (T.Text, T.Text) -> (T.Text, T.Text) -> T.Text
genDCopJson' (d, i) (p, u) = foldl T.append ""
    [ "\n        \"" , d , ":" , p , "\": {\n"
    , "          \"unit\": " , "\"" , u , "\",\n"
    , "          \"reference\": \"" , i , ":" , p , "\",\n"
    , "          \"comment\": \"" , p , " of " , d , "\"\n"
    , "       }," ]

genDCopJson :: ACEConfig -> T.Text
genDCopJson cfg = T.init . T.concat
                $ [ genDCopJson' d p 
                  | d <- zip (dutInstances opID) (dutDevInstances devInst opID)
                  , p <- zip (dcopParameters dco) (dcopUnits dco) ]
    where dco     = dcop cfg
          opID    = aceID cfg
          devInst = deviceInstance cfg

genSizingJson' :: T.Text -> Float -> ACEConstraints -> T.Text
genSizingJson' p v c = foldl T.append ""
        [ "\n    \"" , p , "\" :{\n"
        , "      \"min\":" , min' , ",\n"
        , "      \"init\":" , T.pack (show v) , ",\n"
        , "      \"max\":" , max' , ",\n"
        , "      \"grid\":" , grd' , ",\n"
        , "      \"sizing\": true\n"
        , "    }," ]
    where min' = T.pack . show 
               $ case T.head p of 'L' -> minL c
                                  'W' -> minW c
                                  'M' -> 1.0
                                  _ -> error ("Don't know what " ++ T.unpack p ++ " is")
          max' = T.pack . show 
               $ case T.head p of 'L' -> maxL c
                                  'W' -> maxW c
                                  'M' -> 42.0
                                  _ -> error ("Don't know what " ++ T.unpack p ++ " is")
          grd' = T.pack . show 
               $ case T.head p of 'L' -> gridL c
                                  'W' -> gridW c
                                  'M' -> 1
                                  _ -> error ("Don't know what " ++ T.unpack p ++ " is")

genSizingJson :: ACEConfig -> T.Text
genSizingJson cfg = T.init . T.concat
                  $ [ genSizingJson' p v c 
                    | (p, v, c) <- zip3 (sizingParameters cfg)
                                        (sizingInit cfg)
                                        (repeat . sizingConstraints $ cfg) ]

genParamJson' :: T.Text -> Float -> T.Text
genParamJson' p v = foldl T.append ""
    [ "\n    \"" , p , "\": {\n"
    , "      \"init\": " , T.pack (show v) , ",\n"
    , "      \"comment\": \"I know exactly what " , p , " is!\"\n    }," ]

genParamJson :: ACEConfig -> T.Text
genParamJson cfg = T.concat [ genParamJson' p v
                            | (p, v) <- M.toList . aceParameterMap 
                                                 . parameters 
                                                 $ cfg ]

dutInstances' :: M.Map T.Text [T.Text]
dutInstances' = M.fromList [ ("op1", [ "MNCM11", "MNCM12", "MNCM13", "MND11"
                                     , "MND12", "MPCM21", "MPCM22", "MPCS" ])
                           , ("op2", ["MNCM31", "MNCM32", "MND12", "MND11"
                                     , "MNCM11" ,"MNCM12", "MPCM222", "MPCM221"
                                     , "MPCM212", "MPCM211" ])
                           , ("op3", [ "MNCM31", "MNCM32", "MND12", "MND11"
                                     , "MNCM11", "MNCM12", "MPCM222", "MPCM221"
                                     , "MPCM212","MPCM211" ])
                           , ("op4", [ "MNCM11", "MNCM12", "MNCM13", "MNCM31"
                                     , "MNCM32", "MND12", "MND11", "MPC1R"
                                     , "MPC12", "MPC11", "MPCM222", "MPCM221"
                                     , "MPCM212", "MPCM211" ])
                           , ("op5", [ "MNCM11", "MNCM12", "MNCM13", "MNCM31"
                                     , "MNCM32", "MND12", "MND11", "MPC1R"
                                     , "MPC12", "MPC11", "MPCM222", "MPCM221"
                                     , "MPCM212", "MPCM211" ])
                           , ("op6", [ "MNCM11", "MNCM12", "MNCM13", "MND11"
                                     , "MND12", "MPCM21", "MPCM22", "MPC1"
                                     , "MPR1", "MPCS" ])
                           , ("op8", [ "MND12", "MND11", "MNCM51", "MNCM52"
                                     , "MNCM53", "MPCM41", "MPCM43", "MPCM42"
                                     , "MNCM11", "MNCM12", "MNCM21", "MNCM22"
                                     , "MPCM31", "MPCM32" ])
                           , ("op9", [ "MND11", "MND12", "MNCM11", "MNCM12"
                                     , "MPCM21", "MPCM22", "MNCM41", "MNCM42"
                                     , "MNCM43", "MNCM44", "MPCM31", "MPCM32"
                                     , "MPCM33", "MPCM34", "MNLS12", "MNLS11"
                                     , "MNR1", "MPR2" ])]

dutInstances :: T.Text -> [T.Text]
dutInstances = fromJust . flip M.lookup dutInstances'

dutDevInstances :: ACEInstance -> T.Text -> [T.Text]
dutDevInstances i k = map (\d -> T.concat ["DUT.", d, is d]) di
    where di = dutInstances k
          is d | T.isPrefixOf "MN" d = if T.null (nmosPostFix i)
                                          then ""
                                          else T.concat [".m", nmosPostFix  i]
               | T.isPrefixOf "MP" d = if T.null (pmosPostFix i)
                                          then ""
                                          else T.concat [".m", pmosPostFix  i]
               | otherwise = ""

aceParameterMap :: ACEParameters -> M.Map T.Text Float
aceParameterMap ap = M.fromList $ zip k v
    where v = map ( $ ap) [cl, rl, i0, vs, vsup]
          k = ["cl", "rl", "i0", "vs", "vsup"]

fillPropertiesTemplate :: ACEConfig -> T.Text -> T.Text
fillPropertiesTemplate cfg = T.replace p pj 
                           . T.replace s sj
                           . T.replace o oj
                           . T.replace m mj
    where p = "%PARAMETERS%"
          s = "%SIZING%"
          o = "%DCOP%"
          m = "%DCMATCH%"
          pj = genParamJson cfg
          sj = genSizingJson cfg
          oj = genDCopJson cfg
          mj = genDCmatchJson cfg

genIncludes' :: ACEIncludes -> T.Text
genIncludes' ci = T.concat ["include \"", includeFile ci, "\"",  section , "\n"]
    where section = if T.null $ includeSection ci 
                       then ""
                       else T.append " section=" (includeSection ci)

genIncludes :: ACEConfig -> T.Text
genIncludes cfg = T.concat $ map genIncludes' (includes cfg)

genParameters' :: T.Text -> Float -> T.Text
genParameters' p v = T.concat [ p, "=", T.pack $ show v, " " ]

genParameters :: ACEConfig -> T.Text
genParameters cfg = T.concat
                  $ "parameters " 
                  : [ genParameters' p v
                    | (p, v) <- M.toList . aceParameterMap 
                                         . parameters 
                                         $ cfg ] 
                 ++ ["\n"]

genSizing' :: T.Text -> Float -> T.Text
genSizing' p v = T.concat [ p, "=", T.pack $ show v, " " ]

genSizing :: ACEConfig -> T.Text
genSizing cfg = T.concat
              $ "parameters "
              : [ genSizing' p v
                    | (p, v) <- zip (sizingParameters cfg)
                                        (sizingInit cfg) ]
             ++ [" \\\n\tA=" , area (parameters cfg) , "\n" ]

genSaves :: ACEConfig -> T.Text
genSaves cfg = T.concat $ "save " 
             : [T.concat [ "DUT.M*", di , ":", dc, " \\\n\t" ] 
               | dc <- dco 
               , di <- di' ]
    where inst = deviceInstance cfg
          dco = dcopParameters . dcop $ cfg
          di' = map (\s -> if T.null s then s else T.concat [".m", s])
              $ S.toList $ S.fromList [nmosPostFix inst, pmosPostFix inst]

genHeader :: ACEConfig -> T.Text
genHeader cfg = T.concat [ "// characterization single-ended opamp '"
                         , op, "' in ", pdk]
    where op = aceID cfg
          pdk = technology cfg

genScale :: ACEConfig -> T.Text
genScale cfg = T.concat ["scale=", T.pack $ show (scale cfg)]

fillTestbenchTemplate :: ACEConfig -> T.Text -> T.Text -> T.Text
fillTestbenchTemplate cfg dut = T.replace i i'
                              . T.replace p p'
                              . T.replace s s'
                              . T.replace d d'
                              . T.replace a a'
                              . T.replace h h'
                              . T.replace c c'
    where i = "%INCLUDES%"
          p = "%PARAMETERS%"
          s = "%SIZING%"
          d = "%DUT%"
          a = "%SAVES%"
          h = "%HEADER%"
          c = "%SCALE%"
          i' = genIncludes cfg
          p' = genParameters cfg
          s' = genSizing cfg
          d' = fillDutTemplate (deviceInstance cfg) dut
          a' = genSaves cfg
          h' = genHeader cfg
          c' = genScale cfg

fillDerviedStatements :: ACEInstance -> T.Text -> T.Text
fillDerviedStatements inst dut = T.unlines [ M.foldrWithKey f l' dis 
                                           | l' <- T.lines dut ]
    where dis = M.fromList $ zip (derivedInstances inst) (derivedStatements inst)
          f k v l | T.isInfixOf k l = T.replace "%DERIVED%" 
                                                (T.concat [" \\\n\t\t", v]) l
                  | otherwise       = l

fillDutTemplate :: ACEInstance -> T.Text -> T.Text
fillDutTemplate inst = fillDerviedStatements inst
                     . T.replace n n'
                     . T.replace p p'
                     . T.replace r r'
                     . T.replace c c'
                     . T.replace p p'
    where n = "%NMOS%"
          p = "%PMOS%"
          r = "%RES%"
          c = "%CAP%"
          n' = nmosInstance inst
          p' = pmosInstance inst
          r' = resInstance inst
          c' = capInstance inst
