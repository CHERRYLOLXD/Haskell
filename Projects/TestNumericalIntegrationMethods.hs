module Main where
    
    import Control.Monad (forever)
    
    import Modules.InputValidation.Main (getValidInput)
    
    import Modules.NumericalIntegration.RectangleMethod.Main (rectangleMethod)
    import Modules.NumericalIntegration.SimpsonMethod.Main (simpsonMethod)
    import Modules.NumericalIntegration.TrapezoidalMethod.Main (trapezoidalMethod)
    
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
                    Right result -> putStrLn $ "Result using the rectangle Method: " ++ show result
            2 -> do
                putStrLn "The simpson method is selected."
                lowerLimit <- getValidInput "Enter lower limit:"
                upperLimit <- getValidInput "Enter upper limit:"
                segmentCount <- getValidInput "Enter segment count:"
                case simpsonMethod integralFunction lowerLimit upperLimit segmentCount of
                    Left error -> putStrLn error
                    Right result -> putStrLn $ "Result using the simpson Method: " ++ show result
            3 -> do
                putStrLn "The trapezoidal method is selected."
                lowerLimit <- getValidInput "Enter lower limit:"
                upperLimit <- getValidInput "Enter upper limit:"
                segmentCount <- getValidInput "Enter segment count:"
                case trapezoidalMethod integralFunction lowerLimit upperLimit segmentCount of
                    Left error -> putStrLn error
                    Right result -> putStrLn $ "Result using the trapezoidal Method: " ++ show result
            _ -> putStrLn "Invalid choice. Please try again."
            