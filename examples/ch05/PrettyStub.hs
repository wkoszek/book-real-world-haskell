module PrettyStub where

{-- snippet stubs --}
import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
{-- /snippet stubs --}

{-- snippet append --}
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined
{-- /snippet append --}

{-- snippet hcat --}
hcat :: [Doc] -> Doc
hcat xs = undefined
{-- /snippet hcat --}

{-- snippet fsep --}
fsep :: [Doc] -> Doc
fsep xs = undefined
{-- /snippet fsep --}

hexEscape :: Char -> Doc
hexEscape c = undefined
