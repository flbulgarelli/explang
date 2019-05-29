{
module Language.Mulang.Edl.Parser (parseExpectations, parseQuery) where

import           Language.Mulang.Edl.Expectation
import           Language.Mulang.Edl.Lexer (Token( ..) )
import qualified Language.Mulang.Edl.Lexer as L

import           Control.Monad.Error
}

%name parseExpectations Expectations
%name parseQuery        TopQuery

%monad{L.P}
%lexer{L.lexer}{L.TEOF}
%tokentype{L.Token}
%error {parseError}

%token

  and { TAnd {} }
  atLeast { TAtLeast {} }
  atMost { TAtMost {} }
  cand { TCAnd {} }
  char { TChar {} }
  closeParen  { TCloseParen {} }
  cnot { TCNot {} }
  colon { TColon {} }
  comma { TComma {} }
  cor { TCOr {} }
  count { TCount {} }
  distinct { TDistinct {} }
  exactly { TExactly {} }
  false { TFalse {} }
  identifier { TIdentifier {} }
  in { TIn {} }
  like { TLike {} }
  logic { TLogic {} }
  math { TMath {} }
  nil { TNil {} }
  not { TNot {} }
  number { TNumber {} }
  of { TOf {} }
  openParen  { TOpenParen {} }
  or { TOr {} }
  plus { TPlus {} }
  self { TSelf {} }
  semi { TSemi {} }
  something { TSomething {} }
  string { TString {} }
  symbol { TSymbol {} }
  expectation { TExpectation {} }
  that { TThat {} }
  through { TThrough {} }
  true { TTrue {} }
  with { TWith {} }
  within { TWithin {} }

%left plus
%left or
%left and
%right not
%left cor
%left cand
%right cnot

%%
Expectations :: { [Expectation] }
Expectations : { [] }
  | Expectation { [$1] }
  | Expectation semi Expectations { $1:$3 }

Expectation :: { Expectation }
Expectation : expectation colon TopQuery { Expectation "" $3 }
  | expectation string colon TopQuery { Expectation (stringValue $2) $4 }

TopQuery :: { Query }
TopQuery : Query { $1 }
  | CQuery { Decontextualize $1 }

Query :: { Query }
Query : within symbol CQuery { Within (symbolValue $2) $3 }
  | through symbol CQuery { Through (symbolValue $2) $3 }
  | not openParen Query closeParen { Not $3 }
  | openParen Query closeParen or openParen Query closeParen { Or $2 $6 }
  | openParen Query closeParen and openParen Query closeParen { And $2 $6 }

CQuery :: { CQuery }
CQuery : openParen CQuery closeParen { $2 }
  | Consult { let (i, p, m) = $1 in Inspection i p m }
  | cnot CQuery { CNot $2 }
  | CQuery cand CQuery { CAnd $1 $3 }
  | CQuery cor CQuery { COr $1 $3 }
  | TQuery atLeast Times { AtLeast $3 $1 }
  | TQuery atMost Times { AtMost $3 $1  }
  | TQuery exactly Times { Exactly $3 $1 }

TQuery :: { TQuery }
TQuery : openParen TQuery closeParen { $2 }
  | count openParen Consult closeParen { let (i, p, m) = $3 in Counter i p m }
  | TQuery plus TQuery { Plus $1 $3 }

Times :: { Int }
Times : number { round . numberValue $ $1 }

Inspection :: { String }
Inspection : identifier { identifierValue $1 }
  | Keyword { $1 }
  | identifier Inspection { (identifierValue $1) ++ " " ++ $2 }

Keyword :: { String }
Keyword : and { "and" }
  | count { "count" }
  | false { "false" }
  | logic { "logic" }
  | math { "math" }
  | nil { "nil" }
  | not { "not" }
  | or { "or" }
  | self { "self" }
  | expectation { "expectation" }
  | true { "true" }

Consult :: { (String, Predicate, Matcher) }
Consult : Inspection something Predicate Matcher { ($1, $3, $4) }
  | Inspection Predicate Matcher { ($1, $2, $3) } -- relaxed syntax

Predicate :: { Predicate }
Predicate : { Any }
 | symbol { (Named . symbolValue) $1 }
 | like symbol { (Like . symbolValue) $2 }
 | distinct of symbol { (Except . symbolValue) $3 }
 | in openParen Symbols closeParen  { (AnyOf . map symbolValue) $3 }

Symbols :: { [Token] }
Symbols : symbol { [$1] }
 | symbol comma Symbols { ($1:$3) }

Matcher :: { Matcher }
Matcher : { Unmatching }
  | with Clause { Matching [$2] }
  | with openParen Clauses closeParen { Matching $3 }
  | that openParen TopQuery closeParen { Matching [That $3] } -- relaxed syntax

Clauses :: { [Clause] }
Clauses : Clause { [$1] }
  | Clause comma Clauses { ($1:$3) }

Clause :: { Clause }
Clause : number { IsNumber . numberValue $ $1 }
  | string { IsString . stringValue $ $1 }
  | char { IsChar . charValue $ $1 }
  | symbol { IsSymbol . symbolValue $ $1 }
  | true { IsTrue }
  | false { IsFalse }
  | self { IsSelf }
  | math { IsMath }
  | logic { IsLogic }
  | nil { IsNil }
  | something that openParen TopQuery closeParen { That $4 }
  | that openParen TopQuery closeParen { That $3 } -- relaxed syntax

{
parseError token = throwError ("Parse Error: " ++ m token)

m (TChar v) = "char " ++ show v ++ " is not expected here"
m (TIdentifier id) = "Unexpected keyword " ++ id
m (TNumber v) = "number " ++ show v ++ " is not expected here"
m (TString v) = "string " ++ show v ++ " is not expected here"
m (TSymbol v) = "symbol " ++ v ++ " is not expected here"
m TAnd = "and is not expected here"
m TAtLeast = "least is not expected here"
m TAtMost = "most is not expected here"
m TCAnd = "&& is not expected here"
m TCloseParen = "Unexpected ("
m TCNot = "! is not expected here"
m TComma = "Unexpected ,"
m TCOr = "|| is not expected here"
m TCount = "count is not expected here"
m TDistinct = "distinct is not expected here"
m TEOF = "Unexpected end of file"
m TExactly = "exactly is not expected here"
m TFalse = "false is not expected here"
m TIn = "in is not expected here"
m TLike = "like is not expected here"
m TLogic = "logic is not expected here"
m TMath = "math is not expected here"
m TNil = "nil is not expected here"
m TNot = "not is not expected here"
m TOf = "of is not expected here"
m TOpenParen = "Unexpected )"
m TOr = "or is not expected here"
m TSelf = "self is not expected here"
m TSemi = "Unexpected ;"
m TSomething = "something is not expected here"
m TThat = "that is not expected here"
m TThrough = "through is not expected here"
m TTrue = "true is not expected here"
m TWith = "with is not expected here"
m TWithin = "within is not expected here"
m x =  "Unexpected " ++ show x

}