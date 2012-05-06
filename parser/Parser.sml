structure TaplUntypedLrVals =
  TaplUntypedLrValsFun (structure Token = LrParser.Token)
structure TaplUntypedLex =
  TaplUntypedLexFun(structure Tokens = TaplUntypedLrVals.Tokens)
structure TaplUntypedParser =
  Join (structure LrParser = LrParser
        structure ParserData = TaplUntypedLrVals.ParserData
        structure Lex = TaplUntypedLex)

(* structure TaplUntypedParser = *)
(*   JoinWithArg(structure LrParser = LrParser *)
(*               structure ParserData = TaplUntypedLrVals.ParserData *)
(*               structure Lex = TaplUntypedLex) *)

val tokenstream = TaplUntypedParser.makeLexer
                      (fn _ => TextIO.inputLine TextIO.stdIn)
