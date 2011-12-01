{
module Parse (parse,scan,Stmt(..)) where

import Lex
import Data.List

}

%name parse
%tokentype  {Token}
%error {parseError}

%token
    ';' {TSemicolon}
    '=' {TAssign}
    '+' {TSign $$}
    var {TVar $$}
    const {TConst $$}
    
%%
    
Program         : Statement StatementList       {$1:$2}
StatementList   : {-empty-}                     {[]}
                | Statement StatementList       {$1:$2}
Statement       : var '=' Expression ';'        {Stmt $1 (fst $3) ($1++"="++(snd $3)) }
Expression      : Term                          {$1}
                | Term '+' Expression           {(union (fst $1) (fst $3),(snd $1)++$2++(snd $3))}
Term            : var                           {([$1],$1)}
                | const                         {([],$1)}
                
                
{

parseError :: [Token] -> a
parseError _ = error "Wrong input!"


data Stmt = Stmt String [String] String
    deriving (Eq,Show)
}                