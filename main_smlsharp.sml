_require "smlnj/Basis/IO/text-io.smi"
_require "parser/parser.smi"
_require "eval.smi"

local
  structure P = Parser
  structure E = Eval
in
fun printExp exp = print (P.expToString exp)
fun printResult result = print (E.resultToString result)
fun eval_loop env =
    let
      val exp = P.mainParser ()
      val () = printExp exp
      val (new_env, result) = E.eval env exp
      val () = printResult result
    in
      eval_loop new_env
    end

fun main_loop () =
    let val init_env = Eval.emptyEnv ()
    in  eval_loop init_env end

