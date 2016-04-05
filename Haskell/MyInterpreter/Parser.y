{
module Parser where
import Char
import Syntax
import Lexer

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")
}

%name parse
%tokentype { Token }
%token
  if		{ T_If }
  then		{ T_Then }
  else		{ T_Else }
  let		{ T_Let }
  in		{ T_In }
  int		{ T_Int $$ }
  var		{ T_Var $$ }
  '='		{ T_Eq }
  '+'		{ T_Plus }
  '-'		{ T_Minus }
  '*'		{ T_Times }
  '/'		{ T_Div }
  '('		{ T_OB }
  ')'		{ T_CB }
  ';'		{ T_Semi }
  '>'		{ T_GT }
  '<'		{ T_LT }
  '<='		{ T_LE }
  '>='		{ T_GE }
  '=='		{ T_EQ }
  '/='		{ T_NE }
  bool		{ T_Bool $$ }
  '&&'		{ T_And }
  '||'		{ T_Or }

%right in else
%left '||'
%left '&&'
%nonassoc '>' '<' '<=' '>=' '==' '/='
%left '+' '-'
%left '*' '/'

%%

Exp :: { Expr }
Exp	  : if Exp then Exp else Exp	{ If $2 ($4,$6) }
	  | let Decl Decls in Exp		{ Let ($2:$3) $5 }
	  | Exp '==' Exp				{ Rexpr Equal	  $1 $3 }
	  | Exp '/=' Exp				{ Rexpr NotEqual  $1 $3 }
	  | Exp '>'	 Exp				{ Rexpr Greater	  $1 $3 }
	  | Exp '>=' Exp				{ Rexpr GreaterEq $1 $3 }
	  | Exp '<'	 Exp				{ Rexpr Less	  $1 $3 }
	  | Exp '<=' Exp				{ Rexpr LessEq	  $1 $3 }
	  | Exp '+'	 Exp				{ Bexpr Plus  $1 $3 }
	  | Exp '-'	 Exp				{ Bexpr Minus $1 $3 }
	  | Exp '*'	 Exp				{ Bexpr Times $1 $3 }
	  | Exp '/'	 Exp				{ Bexpr Over  $1 $3 }
	  | Aexp						{ $1 }
	  | '(' Exp ')'					{ $2 }
	  | Exp '&&' Exp				{ If $1 ($3,E_Bool "True") }
	  | Exp '||' Exp				{ If $1 (E_Bool "False",$3) }
	  | Fexp						{ $1 }

Aexp  :	int							{ Num $1 }
	  | bool						{ E_Bool $1 }

Fexp  : var							{ Var $1 }
	  | Fexp Aexp					{ Apply $1 $2 }
	  | Fexp '(' Exp ')'			{ Apply $1 $3 }

Decl :: { Decl }
Decl  :	 var Vars '=' Exp			{ Decl $1 (foldl (flip Fun) $4 $2) }

Decls :	{- empty -}					{ [] }
	  | Decls	';' Decl			{ $1 ++ [$3] }

Vars :: { [Ide] }
Vars  :	{- empty -}					{ [] }
	  | Vars var					{ $2 : $1 }
