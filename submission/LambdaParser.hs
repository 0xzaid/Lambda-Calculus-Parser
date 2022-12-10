module LambdaParser where

import Control.Applicative
import Data.Builder
import Data.Functor
import Data.Lambda  
import Parser
import Prelude

-- You can add more imports if you need them
-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.

-- |
--    Part 1

-- | Exercise 1

{-
Step 1: Construct BNF Grammar
- Lambda ("λ")
- Dot (".")
- Left parenthesis ("(")
- Right parenthesis (")")
- Identifiers (e.g. "x", "y", "z", etc.)
-}

-- | Parses a lambda symbol
lambda :: Parser Char
lambda = is 'λ' <* spaces
--------------------------------------
-- | Parses a dot char
dot :: Parser Char
dot = is '.' <* spaces
--------------------------------------
-- | Parses entire parenthesis
parenthesis :: Parser a -> Parser a
parenthesis = between (is '(' <* spaces) (is ')' <* spaces)
--------------------------------------
-- | Parses char, ignoring the spaces in front of it
char :: Parser a -> Parser a
char c = c <* spaces
--------------------------------------
-- | Parses specific char
charToken :: Char -> Parser Char
charToken c = is c <* spaces
-------------------------------------
-- list of all valid identifiers
allIdentifiers :: [Char]
allIdentifiers = ['a' .. 'z']
--------------------------------------
-- parse one char
oneChar :: Parser Char
oneChar = char $ oneof allIdentifiers
--------------------------------------
-- parse multiple chars/string
manyChars :: Parser String
manyChars = list1 oneChar
--------------------------------------
-- | Parses an all identifiers
identifiers :: Parser Builder
identifiers = foldl1 ap <$> ((term <$>) <$> manyChars)
--------------------------------------
-- | Parses a long lambda parameter expression
lparam :: Parser Char
lparam = between lambda dot oneChar
--------------------------------------
-- | Parses a long lambda parameter expression
long :: Parser Builder
long = liftA2 lam lparam lexpr
--------------------------------------
-- | Parses a long lambda parameters
lterms :: Parser Builder
lterms = identifiers ||| parenthesis identifiers ||| long ||| parenthesis long
--------------------------------------
-- | Parses a long lambda expression
lexpr :: Parser Builder
lexpr = foldl1 ap <$> list1 lterms
--------------------------------------
-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('
longLambdaP :: Parser Lambda
longLambdaP = build <$> lexpr
--------------------------------------
checkSpacing :: Parser a -> Parser a
checkSpacing p = spaces *> p <* spaces
--------------------------------------
-- | Parses a short lambda parameter expression
sparam :: Parser [Char]
sparam = between lambda dot manyChars
--------------------------------------
short :: Parser Builder
short = liftA2 (flip $ foldr lam) sparam sexpr
--------------------------------------
sterms :: Parser Builder
sterms = identifiers ||| short ||| parenthesis sexpr
--------------------------------------
sexpr :: Parser Builder
sexpr = foldl1 ap <$> list1 sterms
--------------------------------------

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

-- Parses a short form lambda calculus expression
shortLambdaP :: Parser Lambda
shortLambdaP = build <$> sexpr

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'

-- shortLambdaP can do both short and long form
lambdaP :: Parser Lambda
lambdaP = shortLambdaP

-- |
--    Part 2

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings
-- Parser that parses one or more space characters.
{-

Church encoding for boolean constructs
TRUE = λxy.x    = K-combinator
FALSE = λxy.y   = K I
IF = λbtf.b t f
AND = λxy. IF x  y FALSE
OR = λxy. IF x TRUE y
NOT = λx. IF x FALSE TRUE

-}

-- Taken from Week 11 tutorial
spaces1 :: Parser String
spaces1 = list1 space

-- Taken from Week 11 tutorial
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        ||| pure a

true :: Builder
true = lam 'x' $ lam 'y' $ term 'x'

false :: Builder
false = lam 'x' $ lam 'y' $ term 'y'
-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

logicP :: Parser Lambda
logicP = undefined

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- Taken from instances.hs in Week 11 tutorial
readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _           -> Nothing

-- | Parse numbers as int until non-digit
-- | Taken from Parser.hs in Week 11 tutorial
int :: Parser Int
int = P f
  where
    -- This is okay because the case statement is small
    f "" = Error UnexpectedEof
    f x = case readInt x of
      Just (v, rest) -> Result rest v
      Nothing -> Error $ UnexpectedChar (head x)

op :: String -> a -> Parser a
op c = ($>) (checkSpacing $ string c)

-- intToLam from Data\Builder.hs
intLam :: Parser Builder
intLam = intToLam <$> int

-- * Precedence * --

prec1 :: [Parser (Builder -> Builder -> Builder)]
prec1 = [add ||| minus]
prec2 :: [Parser (Builder -> Builder -> Builder)]
prec2 = [expo, multiply] ++ prec1

-- * Arithmetic Builders * --
-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | λnfx.n(λgh.h(gf))(λu.x)(λu.u) 
pred1 :: Builder
pred1 = lam 'n' $ lam 'f' $ lam 'x' $ term 'n' `ap` lam 'g' (lam 'h' $ term 'h' `ap` (term 'g' `ap` term 'f')) `ap` lam 'u' (term 'x') `ap` lam 'u' (term 'u')
-- | λnfx.f(nfx)
succ1 :: Builder
succ1 = lam 'n' $ lam 'f' $ lam 'x' (term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x'))
-- | λxy.y succ m
addBuilder :: Builder -> Builder -> Builder
addBuilder x y = lam 'x' (lam 'y' (term 'y' `ap` succ1 `ap` x)) `ap` x `ap` y
-- | λxy.y pred x
minusBuilder :: Builder -> Builder -> Builder
minusBuilder x y = (y `ap` pred1) `ap` x
-- | λxyf.x(yf)
multBuilder :: Builder -> Builder -> Builder
multBuilder x y = lam 'f' $ lam 'x' $ x `ap` (y `ap` term 'f') `ap` term 'x'
-- | λxy.yx
expBuilder :: Builder -> Builder -> Builder
expBuilder = flip ap

-- * Operators * --

-- | Addition
add :: Parser (Builder -> Builder -> Builder)
add = op "+" addBuilder
-- | Subtraction
minus :: Parser (Builder -> Builder -> Builder)
minus = op "-" minusBuilder
-- | Multiplication
multiply :: Parser (Builder -> Builder -> Builder)
multiply = op "*" multBuilder
-- | Exponentiation
expo :: Parser (Builder -> Builder -> Builder)
expo = op "**" (flip ap)




-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> foldl chain arithmeticParam prec1

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
arithmeticParam :: Parser Builder
arithmeticParam = intLam ||| parenthesis arithmeticExpr
arithmeticP :: Parser Lambda
arithmeticP = build <$> foldl chain arithmeticParam prec2
-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- * Comparison operators * -- 


-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
complexCalcP :: Parser Lambda
complexCalcP = undefined

-- |
--    Part 3

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = undefined

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
listOpP :: Parser Lambda
listOpP = undefined

-- | Exercise 2

-- | Implement your function(s) of choice below!
