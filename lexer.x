{

module Lex (scan,Token(..)) where


}
%wrapper "basic"

tokens :- 

    $white+				;
    "+"                 {\s -> TSign "+"}
    "-"                 {\s -> TSign "-"}
    ";"                 {\s -> TSemicolon}
    "="                 {\s -> TAssign}
    [a-z][a-z0-9]*      {\s -> TVar s}  
    [1-9][0-9]*         {\s -> TConst s}
    0                   {\s -> TConst "0"}
    
    
{
data Token = TVar String | TConst String|TSemicolon | TAssign |TSign String deriving (Eq,Show)
    
scan = alexScanTokens
}    