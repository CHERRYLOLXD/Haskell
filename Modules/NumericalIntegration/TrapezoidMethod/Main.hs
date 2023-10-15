module Haskell.Modules.NumericalIntegration.TrapezoidMethod.Main ( trapezoidalMethod ) where

    import Control.Monad ( forever )
    import Haskell.Modules.InputValidation.Main ( getValidInput )

    trapezoidalMethod :: (Double -> Double) -> Double -> Double -> Int -> Double
    trapezoidalMethod integralFunction lowerLimit upperLimit segmentCount =
        (segmentWidth / 2) * (integralFunction lowerLimit + 2 * sum [integralFunction midpoint | midpoint <- [lowerLimit + fromIntegral segmentIndex * segmentWidth | segmentIndex <- [1..segmentCount - 1]]] + integralFunction upperLimit)
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
        putStrLn $ (++) "Result: " $ show $ trapezoidalMethod integralFunction lowerLimit upperLimit segmentCount
