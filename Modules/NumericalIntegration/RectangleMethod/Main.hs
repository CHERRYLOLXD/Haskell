module Haskell.Modules.NumericalIntegration.RectangleMethod.Main ( rectangleMethod ) where

    import Control.Monad ( forever )
    import Haskell.Modules.InputValidation.Main ( getValidInput )

    rectangleMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
    rectangleMethod integralFunction lowerLimit upperLimit segmentCount
        | segmentCount <= 0 = Left "Segment count must be a positive number"
        | otherwise = Right $ segmentWidth * sum [integralFunction midpoint | midpoint <- [lowerLimit + segmentWidth * (fromIntegral segmentIndex + 0.5) | segmentIndex <- [0..segmentCount - 1]]]
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
        case rectangleMethod integralFunction lowerLimit upperLimit segmentCount of
            Left error -> putStrLn error
            Right result -> putStrLn $ "Result: " ++ show result
