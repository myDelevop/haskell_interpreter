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

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) env inp = p env inp

item :: Parser Char
item = P (\env inp -> case inp of
    [] -> []
    (x:xs) -> [(env,x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do {
    x <- item;
    if p x then return x else empty;
 }

space :: Parser ()
space = do {
    many(sat isSpace);
    return ();
}

token :: Parser a -> Parser a
token p = do {
    space;
    v <- p;
    space;
    return v;
}

symbol :: String -> Parser String
symbol xs = token (string xs)

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

ident :: Parser String
ident = 
 do {
  x <- lower;
  xs <- many alphanum;
  return (x:xs);
 }

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int



digit :: Parser Char
digit = sat isDigit


nat :: Parser Int
nat = do {
    xs <- some digit;
    return (read xs);
}

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = 
 do {
  char x;
  string xs;
  return (x:xs);
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
    return (t+a))
    <|>
    (do 
    t <- aterm
    symbol "-"
    a <- aexp
    return (t-a))
    <|>
    aterm

parseAexp :: Parser String
parseAexp = 
    do {
        t <- parseAterm;
        do {
            symbol "+";
            e <- parseAexp;
            return (t ++ "+" ++ e);
        }
        <|>
        do {
            symbol "-";
            e <- parseAexp;
            return (t ++ "-" ++ e);
        }
    } <|> do {
        t <- parseAterm;
        return t;
    }

aterm :: Parser Int
aterm = do {
    f <- afactor;
    symbol "*";
    t <- aterm;
    return (t * f);
    }
    <|>
    afactor

parseAterm :: Parser String
parseAterm = do {
    f <- parseFactor;
    do {
        symbol "*";
        t <- parseAterm;
        return(f ++ "*" ++ t);
    } <|> do {
        symbol "/";
        t <- parseAterm;
        return(f ++ "/" ++ t);
    } <|>
    return f;
}

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

parseFactor :: Parser String
parseFactor = 
    do {
        symbol "(";
        e <- parseAexp;
        symbol ")";
        return ("(" ++ e ++ ")")
    } <|> do {
        symbol "-";
        f <- parseFactor;
        return("-" ++ f);
    } <|> do {
        i <- identifier;
        return i;
    } <|> do {
        i <- integer;
        return (show i);
    }


parseCompareTo :: Parser String
parseCompareTo = do {
    a1 <- parseAexp;
    symbol "<";
    a2 <- parseAexp;
    return (a1 ++ "<" ++ a2);
    }
    <|> do {
        a1 <- parseAexp;
        symbol ">";
        a2 <- parseAexp;
        return (a1 ++ ">" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol "==";
        a2 <- parseAexp;
        return (a1 ++ "==" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol "!=";
        a2 <- parseAexp;
        return (a1 ++ "!=" ++ a2);
    }

bexp :: Parser Bool
bexp = (do
    b0 <- bterm
    symbol "OR"
    b1 <- bexp
    return (b0 || b1))
    <|>
    bterm

bterm :: Parser Bool
bterm = (do 
    f0 <- bfactor
    symbol "AND"
    f1 <- bterm
    return (f0 && f1)
    <|>
    bfactor)

bfactor :: Parser Bool
bfactor = (do
        symbol "True"
        return True)
        <|>
        (do
        symbol "False"
        return False)
        <|>
        (do
        symbol "!"
        b <- bfactor
        return (not b))
        <|>
        (do 
            symbol "("
            b <- bexp
            symbol ")"
            return b)
        <|>
        bcomparison

bcomparison :: Parser Bool
bcomparison = (do 
    a0 <- aexp
    symbol "="
    a1 <- aexp
    return (a0 == a1))
    <|>
    (do 
        a0 <- aexp
        symbol "<="
        a1 <- aexp
        return (a0 <= a1))

parseBexp :: Parser String
parseBexp = do {
    p1 <- parseBexp2;
    symbol "||";
    p2 <- parseBexp;
    return (p1 ++ "||" ++ p2);
    } <|> do {
        p <- parseBexp2;
        return p;
    }

parseBexp2 :: Parser String
parseBexp2 = 
    do {
        p1 <- parseBexp3;
        symbol "&&";
        p2 <- parseBexp2;
        return (p1 ++ "&&" ++ p2);
    }
    <|>
    do {
        c <- parseCompareTo;
        return c;
    }
    <|>
    do {
        symbol "True";
        return "True";
    }
    <|>
    do {
        symbol "False";
        return "False";
    }
    <|>
    do {
        i <- identifier;
        return i;
    }
    <|>
    do {
        symbol "!";
        p <- parseBexp3;
        return ("!" ++ p)
    }

parseBexp3 :: Parser String
parseBexp3 =
    do {
        symbol "(";
        p <- parseBexp;
        symbol ")";
        return ("(" ++ p ++ ")");
    }
    <|>
    do {
        c <- parseCompareTo;
        return c;
    }
    <|>
    do {
        symbol "True";
        return "True";
    }
    <|>
    do {
        symbol "False";
        return "False";
    }
    <|>
    do {
        i <- identifier;
        return i;
    }
    <|>
    do {
        symbol "!";
        p <- parseBexp3;
        return ("!" ++ p)
    }

assignment :: Parser String
assignment = do 
    x <- identifier
    symbol ":="
    v <- aexp
    symbol ";"
    updateEnv Variable{name = x, vtype = "", value = v}

parseAssignment :: Parser String
parseAssignment = 
    do {
        i <- identifier;
        symbol ":=";
        do {
            a <- parseAexp;
            symbol ";";
            return (i ++ ":=" ++ a ++ ";")
        }
    <|>
    do {
        b <- parseBexp;
        symbol ";";
        return (i ++ ":=" ++ b ++ ";");
    }
}


command :: Parser String
command = assignment
    <|>
    ifThenElse
    <|>
    while
    <|>
    (do
        symbol "skip"
        symbol ";")

parseCommand :: Parser String
parseCommand = 
    do {
        a <- parseAssignment;
        return a;
    }
    <|>
    do {
        s <- parseSkip;
        return s;
    }
    <|>
    do {
        i <- parseIfThenElse;
        return i;
    }
    <|>
    do {
        w <- parseWhile;
        return w;
    }
    

program :: Parser String
program = (do 
    command
    program)
    <|>
    command

parseProgram :: Parser String
parseProgram = do {
    c <- parseCommand;
    p <- parseProgram;
    return (c ++ p);
} <|> do {
    c <- parseCommand;
    return c;
}

skip :: Parser String
skip = do {
    symbol "skip";
    symbol ";";
    parseCommand;
}

parseSkip :: Parser String
parseSkip = do {
    symbol "skip";
    symbol ";";
    c <- parseCommand;
    return ("skip;" ++ c)
}


ifThenElse :: Parser String
ifThenElse = (do
    symbol "if"
    b <- bexp
    symbol "{"
    if (b) then
        (do
            program
            symbol "}"
            (do 
                symbol "else"
                symbol "{"
                parseProgram;
                symbol "}"
                return "")
            <|>
            (return ""))
    else
        (do
            parseProgram
            symbol "}"
            (do
                symbol "else"
                symbol "{"
                program
                symbol "}"
                return "")
            <|>
            return "")
        )

parseIfThenElse :: Parser String
parseIfThenElse = do {
    symbol "if";
    symbol "(";
    b <- parseBexp;
    symbol ")";
    symbol "{";
    p1 <- parseProgram;
    symbol "}";
    do {
        symbol "else";
        symbol "}";
        p2 <- parseProgram;
        symbol "}";
        return ("if(" ++ b ++ "){" ++ p1 ++ "}else{" ++ p2 ++ "}")
    }
    <|>
    return ("if(" ++ b ++ "){" ++ p1 ++ "}");
}


while :: Parser String
while = do 
    w <- consumeWhile
    repeatWhile w
    symbol "while"
    -- symbol "("
    b <- bexp
    -- symbol ")"
    symbol "{"
    if (b) then (
        do
            program
            symbol "}"
            repeatWhile w
            while)
    else (
        do
            parseProgram
            symbol "}"
            return "")

parseWhile :: Parser String
parseWhile = do {
    symbol "while";
    symbol "(";
    b <- parseBexp;
    symbol ")";
    symbol "{";
    p <- parseProgram;
    symbol "}";
    return ("while(" ++ b ++ "){" ++ p ++ "}");
}

repeatWhile :: String -> Parser String
repeatWhile c = P(\env input -> [(env, "", c ++ input)])


consumeWhile :: Parser String
consumeWhile = do 
    symbol "while"
    b <- consumeBexp
    symbol "{"
    p <- parseProgram
    symbol "}"
    return ("while " ++ b ++ " {" ++ p ++ "}")


-- ROCL TODOOO
consumeBexp :: Parser String
consumeBexp = do
    return "sd"

main = do 
    print(parse program [] "if(3 > 5) {c:=1+34;}")