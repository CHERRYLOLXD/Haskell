import Control.Monad

chebyshevQuadrature :: (Double -> Double) -> Double -> Double -> Int -> Double
chebyshevQuadrature integralFunction lowerBound upperBound numberOfSegments =
    sum [weight segmentIndex * integralFunction (node segmentIndex) | segmentIndex <- [0..numberOfSegments]] * (upperBound - lowerBound)
    where
        weight :: Int -> Double
        weight segmentIndex
            | segmentIndex == 0 || segmentIndex == numberOfSegments = 1.0 / (2 * fromIntegral numberOfSegments)
            | otherwise = 1.0 / fromIntegral numberOfSegments

        node :: Int -> Double
        node segmentIndex =
            0.5 * (upperBound - lowerBound) * cos (pi * fromIntegral (2 * segmentIndex + 1) / (2 * fromIntegral (numberOfSegments + 1)) + (lowerBound + upperBound) / 2)

main :: IO ()
main =
    forever $ do
        putStrLn "Enter lower bound:"
        lowerBound  <- readLn
        putStrLn "Enter upper bound:"
        upperBound  <- readLn
        putStrLn "Enter number of segments:"
        numberOfSegments <- readLn
        putStrLn $ "Result: " ++ show (chebyshevQuadrature integralFunction lowerBound upperBound numberOfSegments)
        where
            integralFunction :: Double -> Double
            integralFunction x =
                x * x
