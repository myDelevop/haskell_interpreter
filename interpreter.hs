import Control.Applicative 

data Variable = Variable {
    name :: String,
    vtype :: String,
    value :: Int
} deriving Show

type Env = [Variable]

newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env,a,String)]
parse (P p) env inp = p env inp


-- Update the environment with a variable
-- If the variable is new (not declared before), it will be
-- added in the environment 
-- If the variable exstits, its value will be overwritten in.
modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (name x) == (name newVar) then [newVar] ++ xs
                        else [x] ++ modifyEnv xs newVar

updateEnv :: Variable -> Parser String
updateEnv var = P(\env input -> case input of
                    xs -> [((modifyEnv env var), "", xs)])


-- Return the value of a variable given the name
readVariable :: String -> Parser Int
readVariable name = P (\env input -> case searchVariable env name of
                                [] -> []
                                [value] -> [(env, value, input)])

-- Search the value of a variable stored in the Env, given the name
searchVariable :: Env -> String -> [Int]
searchVariable [] queryname = []
searchVariable (x:xs) queryname = 
                            if (name x) == queryname then [(value x)]
                            else searchVariable xs queryname



instance Functor Parser where
    -- fmap :: (a->b) -> Parser a -> Parser b
    fmap g p = P (\env input -> case parse p env input of 
        [] -> []
        [(env, v, out)] -> [(env, g v, out)])


instance Applicative Parser where 
    -- pure :: a -> Parser a
    pure v = P (\env input -> [(env, v, input)])
    -- <*> :: Parser(a -> b) -> Parser a -> Parser b
    pg <*> px = P(\env input -> case parse pg env input of 
        [] -> []
        [(env, g, out)] -> parse(fmap g px) env out)


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\env input -> case parse p env input of
        [] -> []
        [(env, v, out)] -> parse(f v) env out)


empty <|> x = x
x <|> empty = x
x <|> (y <|> z) = (x <|> y) <|> z

main = do 
    print("hello")