structure T = Tokens

%%
%structure TaplUntypedLex
ws = [\ \t\n];

%%
{ws}+ => (lex());
"true" => (T.TRUE);
"false" => (T.FALSE);
"if" => (T.IF);
"then" => (T.THEN);
"else" => (T.ELSE);
"Z" => (T.ZERO);
"S" => (T.SUCC);
"P" => (T.PRED);
"iszero" => (T.ISZERO);
"(" => (T.LPAREN);
")" => (T.RPAREN);
";" => (T.SEMICOLON);
