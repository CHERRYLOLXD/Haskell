module Modules.NumericalIntegration.TrapezoidalMethod.Main (trapezoidalMethod) where

    trapezoidalMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
    trapezoidalMethod integralFunction lowerLimit upperLimit segmentCount
        | segmentCount <= 0 = Left "Segment count must be a positive number."
        | otherwise = Right $ (segmentWidth / 2) * (integralFunction lowerLimit + 2 * sum [integralFunction midpoint | midpoint <- [lowerLimit + fromIntegral segmentIndex * segmentWidth | segmentIndex <- [1..segmentCount - 1]]] + integralFunction upperLimit)
        where

            segmentWidth :: Double
            segmentWidth =
                (upperLimit - lowerLimit) / fromIntegral segmentCount
