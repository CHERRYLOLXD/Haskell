module Haskell.Modules.NumericalIntegration.SimpsonMethod.Main ( simpsonMethod ) where

    import Control.Monad ( forever )
    import Haskell.Modules.InputValidation.Main ( getValidInput )

    simpsonMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
    simpsonMethod integralFunction lowerLimit upperLimit segmentCount
        | odd segmentCount && segmentCount <= 0 = Left "Segment count must be a positive and an even number"
        | odd segmentCount = Left "Segment count must be an even number"
        | segmentCount <= 0 = Left "Segment count must be a positive number"
        | otherwise = Right $ (segmentWidth / 3) * (integralFunction lowerLimit + integralFunction upperLimit + sum [4 * integralFunction (segmentValue segmentIndex) | segmentIndex <- [1, 3..segmentCount - 1]] + sum [2 * integralFunction (segmentValue segmentIndex) | segmentIndex <- [2, 4..segmentCount - 2]])
        where
            segmentWidth :: Double
            segmentWidth =
                (upperLimit - lowerLimit) / fromIntegral segmentCount

            segmentValue :: Int -> Double
            segmentValue segmentIndex =
                lowerLimit + fromIntegral segmentIndex * segmentWidth

    integralFunction :: Double -> Double
    integralFunction value =
        value ^ 2

    main :: IO ()
    main = forever $ do
        lowerLimit <- getValidInput "Enter lower limit:"
        upperLimit <- getValidInput "Enter upper limit:"
        segmentCount <- getValidInput "Enter segment count:"
        case simpsonMethod integralFunction lowerLimit upperLimit segmentCount of
            Left error -> putStrLn error
            Right result -> putStrLn $ "Result: " ++ show result
