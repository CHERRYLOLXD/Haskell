module Main where
    
    import Control.Monad (forever)
    
    import Data.Proxy (Proxy(..))
    import Data.Typeable (typeRep, Typeable)
    import Text.Read (readMaybe)
    
    getValidInput :: forall someType . (Read someType, Typeable someType) => String -> IO someType
    getValidInput prompt = do
        putStrLn prompt
        input <- getLine
        case readMaybe input of
            Just value -> return value
            Nothing -> do
                putStrLn $ "Invalid input. Please enter a valid |" ++ show (typeRep (Proxy :: Proxy someType)) ++ "|."
                getValidInput prompt
                
    rectangleMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
    rectangleMethod integralFunction lowerLimit upperLimit segmentCount
        | segmentCount <= 0 = Left "Segment count must be greater than zero."
        | otherwise = Right $ segmentWidth * sum [integralFunction midpoint | midpoint <- [lowerLimit + segmentWidth * (fromIntegral segmentIndex + 0.5) | segmentIndex <- [0..segmentCount - 1]]]
        where
            
            segmentWidth :: Double
            segmentWidth =
                (upperLimit - lowerLimit) / fromIntegral segmentCount
                
    simpsonMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
    simpsonMethod integralFunction lowerLimit upperLimit segmentCount
        | segmentCount <= 0 = Left "Segment count must be greater than zero."
        | otherwise = Right $ (segmentWidth / 3) * (integralFunction lowerLimit + integralFunction upperLimit
        + sum [4 * integralFunction (segmentValue segmentIndex) | segmentIndex <- [1, 3..segmentCount - 1]]
        + sum [2 * integralFunction (segmentValue segmentIndex) | segmentIndex <- [2, 4..segmentCount - 2]])
        where
            
            segmentWidth :: Double
            segmentWidth =
                (upperLimit - lowerLimit) / fromIntegral segmentCount
                
            segmentValue :: Int -> Double
            segmentValue segmentIndex =
                lowerLimit + fromIntegral segmentIndex * segmentWidth
                
    trapezoidalMethod :: (Double -> Double) -> Double -> Double -> Int -> Either String Double
    trapezoidalMethod integralFunction lowerLimit upperLimit segmentCount
        | segmentCount <= 0 = Left "Segment count must be greater than zero."
        | otherwise = Right $ (segmentWidth / 2) * (integralFunction lowerLimit + 2 * sum [integralFunction midpoint | midpoint <- [lowerLimit + fromIntegral segmentIndex * segmentWidth | segmentIndex <- [1..segmentCount - 1]]] + integralFunction upperLimit)
        where
            
            segmentWidth :: Double
            segmentWidth =
                (upperLimit - lowerLimit) / fromIntegral segmentCount
                
    integralFunction :: Double -> Double
    integralFunction value =
        value ^ 2
        
    main :: IO ()
    main = forever $ do
        putStrLn "Available integration methods:"
        putStrLn "1 -> Rectangle method"
        putStrLn "2 -> Simpson method"
        putStrLn "3 -> Trapezoidal method"
        methodChoice :: Int <- getValidInput "Enter your choice (1, 2, or 3):"
        case methodChoice of
            1 -> do
                putStrLn "The rectangle method is selected."
                lowerLimit <- getValidInput "Enter lower limit:"
                upperLimit <- getValidInput "Enter upper limit:"
                segmentCount <- getValidInput "Enter segment count:"
                case rectangleMethod integralFunction lowerLimit upperLimit segmentCount of
                    Left error -> putStrLn error
                    Right result -> putStrLn $ "Result using the rectangle method: " ++ show result
            2 -> do
                putStrLn "The simpson method is selected."
                lowerLimit <- getValidInput "Enter lower limit:"
                upperLimit <- getValidInput "Enter upper limit:"
                segmentCount <- getValidInput "Enter segment count:"
                case simpsonMethod integralFunction lowerLimit upperLimit segmentCount of
                    Left error -> putStrLn error
                    Right result -> putStrLn $ "Result using the simpson method: " ++ show result
            3 -> do
                putStrLn "The trapezoidal method is selected."
                lowerLimit <- getValidInput "Enter lower limit:"
                upperLimit <- getValidInput "Enter upper limit:"
                segmentCount <- getValidInput "Enter segment count:"
                case trapezoidalMethod integralFunction lowerLimit upperLimit segmentCount of
                    Left error -> putStrLn error
                    Right result -> putStrLn $ "Result using the trapezoidal method: " ++ show result
            _ -> putStrLn "Invalid choice. Please try again."
            