module Alarms (
  Alarms (..),
  AlarmFlag,
  AlarmType (..),
  Severity (..),
  mergeAlarms,
  updateAlarms,
  showAlarms,
  createAlarms,
) where

import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data AlarmType
  = DivByZero
  deriving (Eq, Ord)

instance Show AlarmType where
  show :: AlarmType -> String
  show DivByZero = "division by zero"

data Severity = Possible | Sure deriving (Eq, Ord)

type AlarmFlag = (AlarmType, Severity)

newtype Alarms = Alarms (Map Int (Set AlarmFlag))

showAlarms :: Alarms -> [String]
showAlarms (Alarms alarmsMap) =
  foldr
    ( \(point, flags) acc ->
        let alarmsString = showAlarmSet flags
        in if null alarmsString then acc else ("(" <> show point <> ") " <> alarmsString) : acc
    )
    []
    $ Map.assocs alarmsMap

showAlarmSet :: Set AlarmFlag -> String
showAlarmSet flags = intercalate ", " $ map showAlarmFlag (Set.toList flags)

showAlarmFlag :: AlarmFlag -> String
showAlarmFlag (alarmType, Possible) = "Possible " <> show alarmType
showAlarmFlag (alarmType, Sure) = capitalize $ show alarmType
 where
  capitalize [] = ""
  capitalize (x : xs) = toUpper x : xs

createAlarms :: [Int] -> Alarms
createAlarms points = Alarms $ Map.fromList (map (,Set.empty) points)

mergeAlarms :: Alarms -> Alarms -> Alarms
mergeAlarms (Alarms m1) (Alarms m2) = Alarms $ Map.unionWith Set.union m1 m2

updateAlarms :: Alarms -> Int -> Set AlarmFlag -> Alarms
updateAlarms (Alarms alarmsMap) point newAlarmSet = Alarms $ Map.insertWith Set.union point newAlarmSet alarmsMap