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
    do {
    f <- afactor;
    symbol "/";
    t <- aterm;
    return (f `div` t);
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
    } <|> do { -- possiamo togliere questo do???
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
    symbol "==";
    a2 <- parseAexp;
    return (a1 ++ "==" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol "<=";
        a2 <- parseAexp;
        return (a1 ++ "<=" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol "<";
        a2 <- parseAexp;
        return (a1 ++ "<" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol ">=";
        a2 <- parseAexp;
        return (a1 ++ ">=" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol ">";
        a2 <- parseAexp;
        return (a1 ++ ">" ++ a2);
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
    symbol "=="
    a1 <- aexp
    return (a0 == a1))
    <|>
    (do 
        a0 <- aexp
        symbol "<="
        a1 <- aexp
        return (a0 <= a1))
    <|>
    (do 
        a0 <- aexp
        symbol "<"
        a1 <- aexp
        return (a0 < a1))
    <|>
    (do 
        a0 <- aexp
        symbol ">="
        a1 <- aexp
        return (a0 >= a1))
    <|>
    (do 
        a0 <- aexp
        symbol ">"
        a1 <- aexp
        return (a0 > a1))
    <|>
    (do 
        a0 <- aexp
        symbol "!="
        a1 <- aexp
        return (a0 /= a1))

parseBexp :: Parser String
parseBexp = do {
    p1 <- parseBexp2;
    symbol "OR";
    p2 <- parseBexp;
    return (p1 ++ " OR " ++ p2);
    } <|> do {
        p <- parseBexp2;
        return p;
    }

parseBexp2 :: Parser String
parseBexp2 = 
    do {
        p1 <- parseBexp3;
        symbol "AND";
        p2 <- parseBexp2;
        return (p1 ++ " AND " ++ p2);
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



fromBoolToInt :: Bool -> Int
fromBoolToInt True = 1
fromBoolToInt False = 0


assignment :: Parser String
assignment = (do 
    x <- identifier
    symbol ":="
    v <- aexp
    symbol ";"
    updateEnv Variable{name = x, vtype = "Integer", value = v})
    <|> (do
    x <- identifier
    symbol ":="
    v <- bexp
    symbol ";"
    updateEnv Variable{name = x, vtype = "Boolean", value = (fromBoolToInt v)})

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
    forLoop
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
    <|>
    do {
        f <- parseForLoop;
        return f;
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
    symbol "("
    b <- bexp
    symbol ")"
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
        symbol "{";
        p2 <- parseProgram;
        symbol "}";
        return ("if(" ++ b ++ "){" ++ p1 ++ "}else{" ++ p2 ++ "}")
    }
    <|>
    return ("if(" ++ b ++ "){" ++ p1 ++ "}");
}


forLoop :: Parser String
forLoop = do
    f <- parseForLoop
    repeatWhile f
    program
    return ""


while :: Parser String
while = do 
    w <- consumeWhile
    repeatWhile w
    symbol "while"
    symbol "("
    b <- bexp
    symbol ")"
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
    symbol "while";
    symbol "(";
    b <- consumeBexp
    symbol ")";
    symbol "{";
    p <- parseProgram
    symbol "}"
    return ("while(" ++ b ++ "){" ++ p ++ "}")

consumeBexp :: Parser String
consumeBexp = do
    b <- parseBexp
    return b


parseForLoop :: Parser String
parseForLoop = do {
    symbol "for";
    symbol "(";
    a <- parseAssignment;
    b <- parseBexp;
    symbol ";";
    x <- identifier;
    symbol "++";
    symbol ")";
    symbol "{";
    p <- parseProgram;
    symbol "}";
    return (a ++ " while(" ++ b ++ ") {" ++ p ++ x ++ ":=" ++ x ++ "+1;}");
}



-- Extracts the GML code (it is "a" type) from tuple
getCode :: [(Env, a, String)] -> a
getCode [(_, x, _)]  =  x

booleanType = "Boolean"
integerType = "Integer"

getVarType :: Variable -> String
getVarType = vtype

getVarName :: Variable -> String
getVarName = name

getVarValue :: Variable -> Int
getVarValue = value


-- Extracts the Environment from the tuple and converts in a String form to print it
getMemory :: [(Env, a, String)] -> String
getMemory [] = " Invalid input\n"
getMemory [(x:xs, parsedString, "")] = case Main.getVarType x of
    "Boolean" -> case Main.getVarValue x of
        1 -> " Boolean: " ++ (Main.getVarName x) ++ " = True\n" ++ (getMemory [(xs,parsedString,"")])
        0 -> " Boolean: " ++ (Main.getVarName x) ++ " = False\n" ++ (getMemory [(xs,parsedString,"")])
    "Integer" -> " Integer: " ++ (Main.getVarName x) ++ " = " ++ (show (Main.getVarValue x)) ++ "\n" ++ (getMemory[(xs,parsedString,"")])
    "" -> "Emplty data type but we have following values: " ++ (Main.getVarName x) ++ " = " ++ (show (Main.getVarValue x)) ++ "\n" ++ (getMemory[(xs,parsedString,"")])

getMemory [(env, parsedString, notParsedString)] = case notParsedString of
    "" -> ""
    otherwise -> " Error (unused input '" ++ notParsedString ++ "')\n" ++ getMemory [(env,parsedString, "")]




parser :: String -> IO String
parser xs = 
    do
        putStr "RCInt#>"
        hFlush stdout
        line <- getLine

        case line of
            "printmem" ->
                do
                    putStrLn  ""
                    putStrLn  " ***** Parsed code ***** "
                    if xs == [] then
                        putStrLn "111"
                    else 
                        putStrLn (getCode (parse parseProgram [] xs))
                    putStrLn ""
                    putStrLn "***** Memory *****"
                    putStrLn (getMemory (parse program [] xs))
                    putStrLn ""
                    parser(xs)
            "syntax" ->
                do
                    putStrLn  "***** RC Interpreter - Syntax *****"
                    putStrLn  ""
                    putStrLn  " <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 "
                    putStrLn  ""
                    putStrLn  " <nat> ::= <digit> <nat> | <digit> "
                    putStrLn  ""
                    putStrLn  " <integer> ::= [-] <nat> "
                    putStrLn  ""
                    putStrLn  " <identifier> ::= <lower> | <lower> <alphanum> "
                    putStrLn  ""
                    putStrLn  " <alphanum> ::= <upper> <alphanum> | <lower> <alphanum> | <nat> <alphanum> "
                    putStrLn  "                <upper> | <lower> | <nat> "
                    putStrLn  ""
                    putStrLn  " <lower> ::= a-z "
                    putStrLn  ""
                    putStrLn  " <upper> ::= A-Z "
                    putStrLn  ""
                    putStrLn  " <aexp> ::= <aterm> '+' <aexp> | <aterm> '-' <aexp> | <aterm> "
                    putStrLn  ""
                    putStrLn  " <aterm> ::= <afactor> '*' <aterm> | <afactor> '/' <aterm> | <afactor> "
                    putStrLn  ""
                    putStrLn  " <afactor> ::= '('<aexp>')' | <integer> | <identifier> " 
                    putStrLn  ""
                    putStrLn  " <bexp> ::= <bterm> 'OR' <bexp> | <bterm> "
                    putStrLn  ""
                    putStrLn  " <bterm> ::= <bfactor> 'AND' <bterm> | <bfactor> "
                    putStrLn  ""
                    putStrLn  " <bfactor> ::= 'True' | 'False' | '!'<bfactor> | '('<bexp>')' | <bcomparison> "
                    putStrLn  ""
                    putStrLn  " <bcomparison> ::= <aexp> '==' <aexp> | <aexp> '<=' <aexp> | <aexp> '<' <aexp> | "
                    putStrLn  "                   <aexp> '>=' <aexp> | <aexp> '>' <aexp> | <aexp> '!=' <aexp> "
                    putStrLn  ""
                    putStrLn  " <program> ::= <command> | <command> <program> "
                    putStrLn  ""
                    putStrLn  " <command> ::= <assignment> | <ifThenElse> | <while> | <forLoop> | skip';' "
                    putStrLn  ""
                    putStrLn  " <assignment> ::= <identifier> ':=' <aexp> ';' | <identifier> ':=' <bexp> ';'"
                    putStrLn  ""
                    putStrLn  " <ifThenElse> ::= 'if' '('<bexp>')' '{' <program> '}' |  'if' '('<bexp>')' '{' <program> '}' 'else' '{' <program> '}' "
                    putStrLn  ""
                    putStrLn  " <while> ::= 'while(' <bexp> ') {' <program> '}' "
                    putStrLn  ""
                    putStrLn  " <forLoop> ::= 'for(' <assignment> <bexp> ';' <identifier> '++) { ' <program> '}'"
                    putStrLn  ""
                    parser (xs)
            "help" ->
                do
                    putStrLn  "***** RC-Interpreter Help *****"
                    putStrLn  ""
                    putStrLn  " printmem        => Print the parsed code and the status of the memory"
                    putStrLn  ""
                    putStrLn  " syntax       => Show the BNF grammar for the RC-Interpreter"
                    putStrLn  ""
                    putStrLn  " help            => Print this help"
                    putStrLn  ""
                    putStrLn  " quit exit bye   => Stops RC-Interpreter"
                    putStrLn  ""
                    parser (xs)
            "quit" ->
                do
                    return []
            "exit" ->
                do
                    return []
            "bye" ->
                do
                    return []
            otherwise -> 
                do
                    case parse parseProgram [] line of
                        [] -> 
                            do
                                putStrLn "Syntax error! Please read the syntax typing \"help\" "  
                                parser xs
                        otherwise -> 
                            do
                                parser(xs ++ line)
        return []


rcint :: IO String
rcint = do
    putStrLn "  ──────────────────────────────────────────────────────────────────────────────────────  "
    putStrLn "  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██████╗░░█████╗░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  "
    putStrLn "  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██╔══██╗██╔══██╗░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  "
    putStrLn "  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██████╔╝██║░░╚═╝░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  "
    putStrLn "  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██╔══██╗██║░░██╗░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  "
    putStrLn "  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██║░░██║╚█████╔╝░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  "
    putStrLn "  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░╚═╝░░╚═╝░╚════╝ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  "
    putStrLn "  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  "
    putStrLn "  ██╗███╗░░██╗████████╗███████╗██████╗░██████╗░██████╗░███████╗████████╗███████╗██████╗   "
    putStrLn "  ██║████╗░██║╚══██╔══╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗  "
    putStrLn "  ██║██╔██╗██║░░░██║░░░█████╗░░██████╔╝██████╔╝██████╔╝█████╗░░░░░██║░░░█████╗░░██████╔╝  "
    putStrLn "  ██║██║╚████║░░░██║░░░██╔══╝░░██╔══██╗██╔═══╝░██╔══██╗██╔══╝░░░░░██║░░░██╔══╝░░██╔══██╗  "
    putStrLn "  ██║██║░╚███║░░░██║░░░███████╗██║░░██║██║░░░░░██║░░██║███████╗░░░██║░░░███████╗██║░░██║  "
    putStrLn "  ╚═╝╚═╝░░╚══╝░░░╚═╝░░░╚══════╝╚═╝░░╚═╝╚═╝░░░░░╚═╝░░╚═╝╚══════╝░░░╚═╝░░░╚══════╝╚═╝░░╚═╝  "
    putStrLn "  ──────────────────────────────────────────────────────────────────────────────────────  "
    putStrLn "  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ Rocco Caliandro Interpreter ░░░░░░░░░░░░░░░░░░░░░░░░░░░░  "
    putStrLn "  ──────────────────────────────────────────────────────────────────────────────────────  "
    putStrLn ""
    putStrLn "Type \"help\" for more information"
    putStrLn ""
    putStrLn ""
    parser []

main = do   
    rcint
    -- print(getMemory (parse program [] "a:=3;"))
    -- print(parse program [] "n := 3; i := 0; fact := 1; while (i<n) {fact := fact * (i+1); i := i+1;}")
    -- print(parse program [] "n := 3; i := 0;")
    -- print(parse parseProgram [] "if(1==2 OR 1==2) {a:=1;} else {a:=0;}")
    -- print(parse program [] "n := 3; i := 0; fact := 1; while (i<n OR 1==2) {fact := fact * (i+1); i := i+1;}")
    -- print(parse program [] "a:=0; for(i:=2;i<=3;i++) {a:=a+1;}")

