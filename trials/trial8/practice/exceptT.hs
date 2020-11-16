
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Trans.Except

-- the state automatically adds each
-- new inputted value

runner :: Int -> StateT Int (ExceptT String IO) () -> IO (Either String ())
runner i m = runExceptT $ (evalStateT m) i

countWError :: StateT Int (ExceptT String IO) ()
countWError = run >> countWError
    where 
        run = do
            liftIO $ putStr "Input an integer: "
            i <- liftIO (readLn :: IO Int)
            modify $ \s -> s + i
            s <- get
            lift $ catchE (check s) report
            return ()
        check s = case s `mod` 10 of
                0 -> throwE "Error: The state is divisible by 10"
                _ -> liftIO $ putStrLn $ "The state is now: " ++ show s
        report e = liftIO . putStrLn $ e