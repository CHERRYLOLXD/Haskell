module Modules.NumericalIntegration.RectangleMethod.Main (rectangleMethod) where
    
    rectangleMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
    rectangleMethod integralFunction lowerLimit upperLimit segmentCount
        | segmentCount <= 0 = Left "Segment count must be a positive number."
        | otherwise = Right $ segmentWidth * sum [integralFunction midpoint | midpoint <- [lowerLimit + segmentWidth * (fromIntegral segmentIndex + 0.5) | segmentIndex <- [0..segmentCount - 1]]]
        where
            
            segmentWidth :: Double
            segmentWidth =
                (upperLimit - lowerLimit) / fromIntegral segmentCount
                