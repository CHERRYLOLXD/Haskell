import Control.Monad

rectangleMethod :: (Double -> Double) -> Double -> Double -> Int -> Double
rectangleMethod integralFunction lowerLimit upperLimit segmentCount =
    let segmentWidth = (upperLimit - lowerLimit) / fromIntegral segmentCount
        midpoints = [lowerLimit + segmentWidth * (fromIntegral segmentIndex + 0.5) | segmentIndex <- [0..segmentCount - 1]]
        integralSum = sum [integralFunction midpoint | midpoint <- midpoints]
    in segmentWidth * integralSum

main :: IO ()
main =
    forever $ do
        putStrLn "Enter lower limit:"
        lowerLimit  <- readLn
        putStrLn "Enter upper limit:"
        upperLimit  <- readLn
        putStrLn "Enter segment count:"
        segmentCount <- readLn
        putStrLn $ "Result: " ++ show (rectangleMethod integralFunction lowerLimit upperLimit segmentCount)
        where
            integralFunction :: Double -> Double
            integralFunction value =
                value ^ 2
