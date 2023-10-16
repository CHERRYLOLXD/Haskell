module Modules.NumericalIntegration.SimpsonMethod.Main (simpsonMethod) where

    simpsonMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
    simpsonMethod integralFunction lowerLimit upperLimit segmentCount
        | segmentCount <= 0 = Left "Segment count must be a positive number."
        | otherwise = Right $ (segmentWidth / 3) * (integralFunction lowerLimit + integralFunction upperLimit + sum [4 * integralFunction (segmentValue segmentIndex) | segmentIndex <- [1, 3..segmentCount - 1]] + sum [2 * integralFunction (segmentValue segmentIndex) | segmentIndex <- [2, 4..segmentCount - 2]])
        where

            segmentWidth :: Double
            segmentWidth =
                (upperLimit - lowerLimit) / fromIntegral segmentCount

            segmentValue :: Int -> Double
            segmentValue segmentIndex =
                lowerLimit + fromIntegral segmentIndex * segmentWidth
