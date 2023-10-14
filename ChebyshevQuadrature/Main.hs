import Control.Monad

rectangleMethod :: (Double -> Double) -> Double -> Double -> Int -> Double
rectangleMethod integralFunction lowerLimit upperLimit segmentsCount =
    let segmentWidth = (upperLimit - lowerLimit) / fromIntegral segmentsCount
        midpoints = [lowerLimit + segmentWidth * (fromIntegral i + 0.5) | i <- [0..segmentsCount - 1]]
        integralSum = sum [integralFunction midpoint | midpoint <- midpoints]
    in segmentWidth * integralSum

main :: IO ()
main =
    forever $ do
        putStrLn "Enter lower bound:"
        lowerLimit  <- readLn
        putStrLn "Enter upper bound:"
        upperLimit  <- readLn
        putStrLn "Enter number of segments:"
        segmentsCount <- readLn
        putStrLn $ "Result: " ++ show (rectangleMethod integralFunction lowerLimit upperLimit segmentsCount)
        where
            integralFunction :: Double -> Double
            integralFunction x =
                x * x
