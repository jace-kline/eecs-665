module Parser where
    
import Control.Monad
import Control.Applicative

newtype Parser t a = Parser { runParser :: [t] -> Either String (a, [t]) }

instance Functor (Parser t) where
    fmap f (Parser g) = Parser $ \ts -> fmap (\(x,y) -> (f x, y)) $ g ts

instance Applicative (Parser t) where
    pure x = Parser $ \ts -> pure (x, ts)
    liftA2 f (Parser g) (Parser h) = Parser $ \ts -> do
        (x, ts')  <- g ts
        (y, ts'') <- h ts'
        return (f x y, ts'')

instance Alternative (Parser t) where
    empty = Parser $ \_ -> Left "Parse failed on disjunction options"
    (Parser g) <|> (Parser h) = Parser $ \ts -> 
        case g ts of
            Left m   -> h ts
            x        -> x

instance Monad (Parser t) where
    return = pure
    (Parser g) >>= h = Parser $ \ts -> 
        case g ts of
            Right (x, ts') -> (runParser $ h x) ts'
            Left es        -> Left es

instance MonadFail (Parser t) where
    fail msg = Parser $ \_ -> Left msg


combine :: Parser t a -> Parser t [a] -> Parser t [a]
combine = liftA2 (:)

someWithSeparator :: Parser t a -> Parser t b -> Parser t [a]
someWithSeparator f sep = (liftA3 (\x _ z -> x : z) f sep (someWithSeparator f sep)) 
                        <|> fmap (\x -> [x]) f

appSum :: (Alternative f) => [f a] -> f a                      
appSum = foldr (<|>) empty

failWithContext :: String -> ([t] -> String) -> Parser t a
failWithContext msg f = Parser $ \ts -> Left $ msg ++ f ts

zeroOrMore :: Parser t a -> Parser t [a]
zeroOrMore p = (pure (:) <*> p <*> zeroOrMore p) <|> ((pure :: a -> [a]) p) <|> pure []

oneOrMore :: Parser t a -> Parser t [a]
oneOrMore p = (pure (:) <*> p <*> oneOrMore p) <|> ((pure :: a -> [a]) p) <|> empty 
