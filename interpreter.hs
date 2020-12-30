import Control.Applicative 
import Data.Char
import System.IO


data Variable = Variable {
    name :: String,
    vtype :: String,
    value :: Int
} deriving Show

type Env = [Variable]

newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env,a,String)]
parse (P p) env inp = p env inp


symbol :: String -> Parser String
symbol xs = token (string xs)


identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int


char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat isDigit

ident :: Parser String
ident = 
 do {
  x <- lower;
  xs <- many alphanum;
  return (x:xs);
 }


string :: String -> Parser String
string [] = return []
string (x:xs) = 
 do {
  char x;
  string xs;
  return (x:xs);
}

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum


nat :: Parser Int
nat = 
 do {
  xs <- some digit;
  return (read xs);
 }
int :: Parser Int
int = 
 do {
  char '-';
  n <- nat;
  return (-n);
 }
 <|>
 nat;

token :: Parser a -> Parser a
token p = 
 do {
  space;
  v <- p;
  space;
  return v;
 }


space :: Parser ()
space = 
 do {
  many (sat isSpace);
  return ();
}

item :: Parser Char
item = P (\env inp -> case inp of 
 [] -> []
 (x:xs) -> [(env,x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = 
 do {
 x <- item;
 if p x then return x else empty;
 }


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

{-
empty <|> x = x
x <|> empty = x
x <|> (y <|> z) = (x <|> y) <|> z
-}

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\env input -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\env input -> case parse p env input of 
        [] -> parse q env input
        [(env, v, out)] -> [(env, v, out)])


aexp :: Parser Int
aexp = (do 
    t <- aterm
    symbol "+"
    a <- aexp
    return (t+1))
    <|>
    (do 
    t <- aterm
    symbol "-"
    a <- aexp
    return (t-a))
    <|>
    aterm

aterm :: Parser Int
aterm = do {
    f <- afactor
    ; symbol "*"
    ; t <- aterm
    ; return (t * f)
    }
    <|>
    afactor


afactor :: Parser Int
afactor = (do 
    symbol "("
    a <- aexp
    symbol ")"
    return a)
    <|>
    (do 
    i <- identifier
    readVariable i)
    <|>
    integer

main = do 
    print("hello")