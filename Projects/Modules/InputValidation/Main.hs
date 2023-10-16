module Modules.InputValidation.Main where

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
