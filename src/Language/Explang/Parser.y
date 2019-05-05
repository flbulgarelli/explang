{
module Language.Explang.Parser (parse) where

import           Language.Explang.Expectation
import           Language.Explang.Lexer (Token( ..) )
import qualified Language.Explang.Lexer as L

import           Control.Monad.Error
}

%monad{L.P}
%lexer{L.lexer}{L.TEOF}
%name parse
%tokentype{L.Token}
%error {parseError}

%token

  and { TAnd {} }
  any { TAny {} }
  at { TAt {} }
  char { TChar {} }
  closeParen  { TCloseParen {} }
  comma { TComma {} }
  distinct { TDistinct {} }
  exactly { TExactly {} }
  false { TFalse {} }
  identifier { TIdentifier {} }
  intransitively { TIntransitively {} }
  least { TLeast {} }
  like { TLike {} }
  logic { TLogic {} }
  math { TMath {} }
  most { TMost {} }
  nil { TNil {} }
  not { TNot {} }
  number { TNumber {} }
  of { TOf {} }
  openParen   { TOpenParen {} }
  or { TOr {} }
  self { TSelf {} }
  semi { TSemi {} }
  something { TSomething {} }
  string { TString {} }
  symbol { TSymbol {} }
  that { TThat {} }
  times { TTimes {} }
  true { TTrue {} }
  with { TWith {} }
  within { TWithin {} }

%left or
%left and
%right not

%%
Expectations :: { [Expectation] }
Expectations : { [] }
  | Expectation { [$1] }
  | Expectation semi Expectations { $1:$3 }

Expectation :: { Expectation }
Expectation : Flags Scope Query Count { Expectation $1 $2 $3 $4 }

Flags :: { Flags }
Flags : { noFlags }
  | intransitively { intransitiveFlag }

Scope :: { Scope }
Scope : { Unscoped }
  | within symbol { (Scoped . symbolValue) $2 }

Query :: { Query }
Query : Inspection Binding Matcher { Inspection $1 $2 $3 }
  | not Query { Not $2 }
  | Query or Query { Or $1 $3 }
  | Query and Query { And $1 $3 }
  | openParen Query closeParen { $2 }

Inspection :: { String }
Inspection : identifier { identifierValue $1 }
  | identifier Inspection { (identifierValue $1) ++ " " ++ $2 }

Binding :: { Binding }
Binding : { Any }
 | symbol { (Named . symbolValue) $1 }
 | something like symbol { (Like . symbolValue) $3 }
 | like symbol { (Like . symbolValue) $2 } -- relaxed syntax
 | something distinct of symbol { (Except . symbolValue) $4 }
 | distinct of symbol { (Except . symbolValue) $3 } -- relaxed syntax
 | any of openParen Symbols closeParen  { (AnyOf . map symbolValue) $4 }

Symbols :: { [Token] }
Symbols : symbol { [$1] }
 | symbol comma Symbols { ($1:$3) }

Matcher :: { Matcher }
Matcher : { Unmatching }
  | with Predicate { Matching [$2] }
  | with openParen Predicates closeParen { Matching $3 }
  | that openParen Expectation closeParen { Matching [That $3] } -- relaxed syntax

Predicates :: { [Predicate] }
Predicates : Predicate { [$1] }
  | Predicate comma Predicates { ($1:$3) }

Predicate :: { Predicate }
Predicate : number { IsNumber . numberValue $ $1 }
  | string { IsString . stringValue $ $1 }
  | char { IsChar . charValue $ $1 }
  | symbol { IsSymbol . symbolValue $ $1 }
  | true { IsTrue }
  | false { IsFalse }
  | self { IsSelf }
  | math { IsMath }
  | logic { IsLogic }
  | nil { IsNil }
  | something that openParen Expectation closeParen { That $4 }
  | that openParen Expectation closeParen { That $3 } -- relaxed syntax

Count :: { Count }
Count : { AnyCount }
  | at least number times { AtLeast . round . numberValue $ $3 }
  | at most number times { AtMost . round . numberValue $ $3 }
  | exactly number times { Exactly . round . numberValue $ $2 }


{
parseError token = throwError ("Parse Error: " ++ m token)

m (TIdentifier id) = "Unexpected keyword " ++ id
m (TString v) = "string " ++ show v ++ " is not expected here"
m (TSymbol v) = "symbol " ++ v ++ " is not expected here"
m (TNumber v) = "number " ++ show v ++ " is not expected here"
m (TChar v) = "char " ++ show v ++ " is not expected here"
m TEOF = "Unexpected end of file"
m TSemi = "Unexpected ;"
m TComma = "Unexpected ,"
m TOpenParen = "Unexpected )"
m TCloseParen = "Unexpected ("
m TAnd = "and is not expected here"
m TAny = "any is not expected here"
m TAt = "at is not expected here"
m TDistinct = "distinct is not expected here"
m TExactly = "exactly is not expected here"
m TFalse = "false is not expected here"
m TIntransitively = "intransitively is not expected here"
m TLeast = "least is not expected here"
m TLike = "like is not expected here"
m TLogic = "logic is not expected here"
m TMath = "math is not expected here"
m TMost = "most is not expected here"
m TNil = "nil is not expected here"
m TNot = "not is not expected here"
m TOf = "of is not expected here"
m TOr = "or is not expected here"
m TSelf = "self is not expected here"
m TSomething = "something is not expected here"
m TThat = "that is not expected here"
m TTimes = "times is not expected here"
m TTrue = "true is not expected here"
m TWith = "with is not expected here"
m TWithin = "within is not expected here"
m x =  "Unexpected " ++ show x

}
