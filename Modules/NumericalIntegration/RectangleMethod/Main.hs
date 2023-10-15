module Haskell.Modules.NumericalIntegration.RectangleMethod.Main (rectangleMethod) where

import Control.Monad ( forever )
import Haskell.Modules.InputValidation.Main (getValidInput)

rectangleMethod :: (Double -> Double) -> Double -> Double -> Int -> Double
rectangleMethod integralFunction lowerLimit upperLimit segmentCount =
    segmentWidth * sum [integralFunction midpoint | midpoint <- [lowerLimit + segmentWidth * (fromIntegral segmentIndex + 0.5) | segmentIndex <- [0..segmentCount - 1]]]
    where
        segmentWidth :: Double
        segmentWidth =
            (upperLimit - lowerLimit) / fromIntegral segmentCount

integralFunction :: Double -> Double
integralFunction value =
    value ^ 2

main :: IO ()
main = forever $ do
    lowerLimit <- getValidInput "Enter lower limit:"
    upperLimit <- getValidInput "Enter upper limit:"
    segmentCount <- getValidInput "Enter segment count:"
    putStrLn $ "Result: " ++ show (rectangleMethod integralFunction lowerLimit upperLimit segmentCount)
