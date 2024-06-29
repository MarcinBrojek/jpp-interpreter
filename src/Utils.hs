module Utils where

import AbsGrammar

genPosInfo :: BNFC'Position -> String
genPosInfo (Just (line, column)) = show line ++ ":" ++ show column ++ ": "
genPosInfo Nothing = ""

genTypeInfo :: Show t => t -> String
genTypeInfo t = "'" ++ show t ++ "'"

genTypesInfo :: Show t => [t] -> String
genTypesInfo [] = ""
genTypesInfo [t] = genTypeInfo t
genTypesInfo (t:ts) = genTypeInfo t ++ ", " ++ genTypesInfo ts
