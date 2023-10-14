import Control.Monad

simpsonMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
simpsonMethod integralFunction lowerLimit upperLimit segmentCount
| odd segmentCount && segmentCount <= 0 = Left "Segment count must be a positive and an even number"
| odd segmentCount = Left "Segment count must be a positive number"
| segmentCount <= 0 = Left "Segment count must be an even number"
| otherwise = Right integralSum
where
    segmentWidth = (upperLimit - lowerLimit) / fromIntegral segmentCount
    segmentValue segmentIndex = lowerLimit + fromIntegral segmentIndex * segmentWidth
    midpoints = sum [4 * integralFunction (segmentValue segmentIndex) | segmentIndex <- [1, 3..segmentCount - 1]] + sum [2 * integralFunction (segmentValue segmentIndex) | segmentIndex <- [2, 4..segmentCount - 2]]
    integralSum = (segmentWidth / 3) * (integralFunction lowerLimit + integralFunction upperLimit + midpoints)

main :: IO ()
main =
    forever $ do
        putStrLn "Enter lower limit:"
        lowerLimitS  <- readLn
        putStrLn "Enter upper limit:"
        upperLimitS  <- readLn
        putStrLn "Enter segment count:"
        segmentCountS <- readLn
        let lowerLimit = read lowerLimitS :: Double
        let upperLimit = read upperLimitS :: Double
        let segmentCount = read segmentCountS :: Int
        case simpsonMethod integralFunction lowerLimit upperLimit segmentCount of
            Left error -> putStrLn error
            Right result -> putStrLn $ "Result: " ++ show result
        where
            integralFunction :: Double -> Double
            integralFunction value =
                value ^ 2
