{
module Parser where
import Token
import Ast
import Data.Map (Map)
import qualified Data.Map as Map
import Factors
import GHC.Float

}

%name parser
%tokentype { Token }
%error { parseError }
%monad { Either String }

%token
      let             { TokenLet }
      return          { TokenReturn }
      as              { TokenAs }
      in              { TokenIn }
      per             { TokenPer }
      good            { TokenGood }
      bad             { TokenBad }
      double          { TokenDouble $$ }
      string          { TokenString $$ }
      int             { TokenInt $$ }
      ident           { TokenIdent $$ }
      ';'             { TokenSemicolon }
      ','             { TokenComma }
      '='             { TokenEqual }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDivide }
      '%'             { TokenModulo }
      '^'             { TokenCaret }
      '('             { TokenLeftParen }
      ')'             { TokenRightParen }
      '{'             { TokenLeftBrace }
      '}'             { TokenRightBrace}
      '<'             { TokenLess }
      '<='            { TokenLessEqual }
      '>'             { TokenGreater }
      '>='            { TokenGreaterEqual }

%right in
%nonassoc '>' '<' '<=' '>='
%nonassoc as
%left '+' '-'
%left '*' '/' '%'
%left NEG

%%

Block : BlockRev { reverse $1 }

BlockRev : {- empty -}     { [] }
         | BlockRev Stmt   { $2 : $1 }

Stmt : let ident '=' Exp ';'      { Let $2 $4 }
     | return Exp ';'             { Return $2 }

UnitsTerm : ident                              { UnitsIdent $1 }
          | ident '^' int                      { UnitsPow $1 $3 }

UnitsSide : UnitsTerm                        { [ $1 ] }
          | UnitsSide UnitsTerm              { $2 : $1 }

UnitsExp : UnitsSide                    { UnitsProd (reverse $1) }
         | UnitsSide per UnitsSide      { UnitsPer (reverse $1) (reverse $3) }

ExpListRev : Exp                  { [ $1 ] }
           | ExpListRev ',' Exp   { $3 : $1 }

ExpList : ExpListRev { reverse $1 }

Option : good Exp       { Good $2 }
       | bad Exp string { Bad $2 $3 }

OptionListRev : Option                { [ $1 ] }
              | OptionList ',' Option { $3 : $1 }

OptionList : OptionListRev { reverse $1 }

Exp   : Exp '+' Exp             { Plus $1 $3 }
      | Exp '-' Exp             { Minus $1 $3 }
      | Exp '*' Exp             { Times $1 $3 }
      | Exp '/' Exp             { Divide $1 $3 }
      | Exp '%' Exp             { Modulo $1 $3 }
      | Exp '<' Exp             { Less $1 $3 }
      | Exp '<=' Exp            { LessEqual $1 $3 }
      | Exp '>' Exp             { Greater $1 $3 }
      | Exp '>=' Exp            { GreaterEqual $1 $3 }
      | Exp in UnitsExp         { AsUnits $1 $3 }
      | Exp UnitsExp            { WithUnits $1 $2 }
      | ident '(' ExpList ')'   { Call $1 $3 }
      | '{' OptionList '}'      { Union $2 }
      | '(' Exp ')'             { Parens $2 }
      | '-' Exp %prec NEG       { Negate $2 }
      | double                  { Double $1 }
      | int                     { Double (int2Double $1) }
      | ident                   { Ident $1 }

{
parseError :: [Token] -> Either String a
parseError tokens = Left $ "Parse error at " ++ show tokens

}