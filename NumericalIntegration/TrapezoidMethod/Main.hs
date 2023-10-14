import Control.Monad

trapezoidalMethod :: (Double -> Double) -> Double -> Double -> Int -> Double
trapezoidalMethod integralFunction lowerLimit upperLimit segmentCount =
    let segmentWidth = (upperLimit - lowerLimit) / fromIntegral segmentCount
        midpoints = [lowerLimit + fromIntegral segmentIndex * segmentWidth | segmentIndex <- [1..segmentCount - 1]]
        integralSum = (segmentWidth / 2) * (integralFunction lowerLimit + 2 * sum [integralFunction midpoint | midpoint <- midpoints] + integralFunction upperLimit)
    in integralSum

main :: IO ()
main =
    forever $ do
        putStrLn "Enter lower limit:"
        lowerLimit  <- readLn
        putStrLn "Enter upper limit:"
        upperLimit  <- readLn
        putStrLn "Enter segment count:"
        segmentCount <- readLn
        putStrLn $ "Result: " ++ show (trapezoidalMethod integralFunction lowerLimit upperLimit segmentCount)
        where
            integralFunction :: Double -> Double
            integralFunction value =
                value ^ 2
