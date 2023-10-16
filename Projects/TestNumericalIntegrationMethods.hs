module Main where

    import Control.Monad (forever)

    import Modules.InputValidation.Main (getValidInput)

    import Modules.NumericalIntegration.RectangleMethod.Main (rectangleMethod)
    import Modules.NumericalIntegration.SimpsonMethod.Main (simpsonMethod)
    import Modules.NumericalIntegration.TrapezoidMethod.Main (trapezoidMethod)

    integralFunction :: Double -> Double
    integralFunction value =
        value ^ 2

    main :: IO ()
    main = forever $ do
        putStrLn "Available integration methods:"
        putStrLn "1 -> Rectangle method"
        putStrLn "2 -> Simpson method"
        putStrLn "3 -> Trapezoid method"
        methodChoice :: Int <- getValidInput "Enter your choice (1, 2, or 3):"
        case methodChoice of
            1 -> do
                lowerLimit <- getValidInput "Enter lower limit:"
                upperLimit <- getValidInput "Enter upper limit:"
                segmentCount <- getValidInput "Enter segment count:"
                case rectangleMethod integralFunction lowerLimit upperLimit segmentCount of
                    Left error -> putStrLn error
                    Right result -> putStrLn $ "Result using Rectangle Method: " ++ show result
            2 -> do
                lowerLimit <- getValidInput "Enter lower limit:"
                upperLimit <- getValidInput "Enter upper limit:"
                segmentCount <- getValidInput "Enter segment count:"
                case simpsonMethod integralFunction lowerLimit upperLimit segmentCount of
                    Left error -> putStrLn error
                    Right result -> putStrLn $ "Result using Simpson Method: " ++ show result
            3 -> do
                lowerLimit <- getValidInput "Enter lower limit:"
                upperLimit <- getValidInput "Enter upper limit:"
                segmentCount <- getValidInput "Enter segment count:"
                case trapezoidMethod integralFunction lowerLimit upperLimit segmentCount of
                    Left error -> putStrLn error
                    Right result -> putStrLn $ "Result using Trapezoid Method: " ++ show result
            _ -> putStrLn "Invalid choice. Please try again."
