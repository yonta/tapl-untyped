
local
  structure A = Absyn
  structure T = TextIO
in
structure Parser
(*
          : sig
            type 'a result
            type 'a table
            val parser : (unit -> char option) -> unit -> A.exp
            val stringParser : string -> A.exp * string
            val mainParser : unit -> A.exp
          end
*)
  =
struct
  fun curry f x y = f (x,y)
  fun uncurry f (x,y) = f x y

  datatype 'a result =
           Success of 'a * table ref | Miss | No
       and table =
           Table of {
             char : char result ref,
             token : string result ref,
             true : A.exp result ref,
             false : A.exp result ref,
             ifexp : A.exp result ref,
             zero : A.exp result ref,
             succ : A.exp result ref,
             pred : A.exp result ref,
             iszero : A.exp result ref,
             exp : A.exp result ref
           }
  fun appResult f (Success (x, t)) = Success (f x, t)
    | appResult _ Miss = Miss
    | appResult _ No = No
  fun look field nowtable =
      case ! nowtable of
          Table table => ! (field table)
  fun update field nowtable result =
      case ! nowtable of
          Table table => (field table) := result
  fun newTable () =
      (ref (Table {
               char = ref No,
               token = ref No,
               true = ref No,
               false = ref No,
               ifexp = ref No,
               zero = ref No,
               succ = ref No,
               pred = ref No,
               iszero = ref No,
               exp = ref No
      })) : table ref
  infix ++ ** *** || *> <*
  fun (parser1 ++ parser2) get1 table =
      case parser1 get1 table of
        Success (exp1, nextTable) => Success (exp1, nextTable)
      | Miss => parser2 get1 table
      | No => raise Fail "Bug: ++"
  fun (parser1 *> parser2) get1 table =
      case parser1 get1 table of
          Success (exp1, nextTable) =>
          (case parser2 get1 nextTable of
               Success (exp2, resultTable) =>
               Success (exp2, resultTable)
             | Miss => Miss
             | No => raise Fail "Bug: *>")
        | Miss => Miss
        | No => raise Fail "Bug: *>"
  fun (parser1 <* parser2) get1 table =
      case parser1 get1 table of
          Success (exp1, nextTable) =>
          (case parser2 get1 nextTable of
               Success (exp2, resultTable) =>
               Success (exp1, resultTable)
             | Miss => Miss
             | No => raise Fail "Bug: <*")
        | Miss => Miss
        | No => raise Fail "Bug: <*"
  fun (parser1 ** parser2) mixer get1 table =
      case parser1 get1 table of
          Success (exp1, nextTable) =>
          (case parser2 get1 nextTable of
               Success (exp2, resultTable) =>
               Success (mixer exp1 exp2, resultTable)
             | Miss => Miss
             | No => raise Fail "Bug: **")
        | Miss => Miss
        | No => raise Fail "Bug: **"
  fun (parser1 *** parser2) get1 table =
      case parser1 get1 table of
          Success (exp1, nextTable) =>
          (case parser2 get1 nextTable of
               Success (exp2, resultTable) =>
               Success (exp1 ^ exp2, resultTable)
             | Miss => Miss
             | No => raise Fail "Bug: ***")
        | Miss => Miss
        | No => raise Fail "Bug: ***"
  (*
   *  note: take care if arguments have No or not.
   *        this function "||" don't think No result.
   *)
  fun (x as Success _) || _ = x
    | Miss || (x as Success _) = x
    | _ || _ = Miss
  fun parseChar get1 table =
      case look #char table of
        x as Success (c, next) => x
      | Miss => raise Fail "parseCharPrim: Miss"
      | No => case get1 () of
                  SOME c =>
                  let val result = Success (c, newTable ())
                  in update #char table result; result end
                | NONE => raise Fail "parseCharPrim: cannot get input character"
  fun skipSpace get1 table =
      case parseChar get1 table of
          x as Success (c, next) =>
          if Char.isSpace c then skipSpace get1 next
          else Success ((), table)
        | Miss => raise Fail "skipSpace: Miss"
        | No => raise Fail "skipSpace: No"
  fun eqCharResult c (Success (c', _)) = c = c'
    | eqCharResult _ _ = false
  fun parseEqChar c get1 table =
      let
        val charresult = parseChar get1 table
      in
        if eqCharResult c charresult then charresult
        else Miss
      end
  fun resultConcat (Success (c1, _)) (Success (c2, table)) =
      Success (c1 ^ c2, table)
    | resultConcat _ Miss = Miss
    | resultConcat _ _ = raise Fail "resultConcat: not success"
  fun parseEqCharlist nil _ nowtable = Success ("", nowtable)
    | parseEqCharlist (c::rest) get1 (nowTable : table ref) =
      case parseEqChar c get1 nowTable of
          x as Success (c', nextTable) =>
          resultConcat (appResult str x) (parseEqCharlist rest get1 nextTable)
        | Miss => Miss
        | No => raise Fail "parseEqCharlist: No"
  fun parseToken get1 table =
      case look #token table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let val result = (skipSpace *>
                            (parseEqCharlist (explode "true") ++
                             parseEqCharlist (explode "false") ++
                             parseEqCharlist (explode "if") ++
                             parseEqCharlist (explode "then") ++
                             parseEqCharlist (explode "else") ++
                             parseEqCharlist (explode "0") ++
                             parseEqCharlist (explode "succ") ++
                             parseEqCharlist (explode "pred") ++
                             parseEqCharlist (explode "iszero"))) get1 table
          in update #token table result; result end
  fun eqStrResult str (Success (str', _)) = str = str'
    | eqStrResult _ _ = false
  fun parseTokenStr str get1 table =
      let val result = parseToken get1 table
      in if eqStrResult str result then result else Miss end
  fun parseFalse get1 table =
      case look #false table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let
            val result = (parseTokenStr "false") get1 table
            val result' = appResult (fn _ => A.False) result
          in
            update #false table result'; result'
          end
  fun parseTrue get1 table =
      case look #true table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let
            val result = (parseTokenStr "true") get1 table
            val result' = appResult (fn _ => A.True) result
          in
            update #true table result'; result'
          end
  fun parseZero get1 table =
      case look #zero table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let
            val result = (parseTokenStr "0") get1 table
            val result' = appResult (fn _ => A.Zero) result
          in
            update #zero table result'; result'
          end
  fun parseIszero get1 table =
      case look #iszero table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let
            val result = (parseTokenStr "iszero" *> parseExp) get1 table
            val result' = appResult A.IsZero result
          in
            update #iszero table result'; result'
          end
  and parsePred get1 table =
      case look #pred table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let
            val result = (parseTokenStr "pred" *> parseExp) get1 table
            val result' = appResult A.Pred result
          in
            update #pred table result'; result'
          end
  and parseSucc get1 table =
      case look #succ table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let
            val result = (parseTokenStr "succ" *> parseExp) get1 table
            val result' = appResult A.Succ result
          in
            update #succ table result'; result'
          end
  and parseIfexp get1 table =
      case look #ifexp table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let
            val result =
                (((((parseTokenStr "if") *> parseExp <* parseTokenStr "then") **
                     parseExp) (fn exp1 => fn exp2 => (exp1, exp2)) <*
                     (parseTokenStr "else") ** parseExp)
                   (fn (exp1, exp2) => fn exp3 => (exp1, exp2, exp3)))
                  get1 table
            val result' = appResult A.If result
          in
            update #ifexp table result'; result'
          end
  and parseExp get1 table =
      case look #exp table of
          x as Success _ => x
        | Miss => Miss
        | No =>
          let val result = (parseIfexp ++ parseSucc ++ parsePred ++
                            parseIszero ++ parseZero ++ parseTrue ++
                            parseFalse) get1 table
          in update #exp table result; result end
  fun parse get1 () =
      let
        val initTable = newTable ()
      in
        case parseExp get1 initTable of
            Success (exp, next) => SOME (exp, next)
          | Miss => NONE
          | No => raise Fail "parse: No"
      end
  val consumeInput = ref ""
  fun get1string () =
      let
        val nowInput = !consumeInput
        val head = String.sub (nowInput, 0)
        val tail = String.substring (nowInput, 1, size nowInput - 1)
        val () = consumeInput := tail
      in
        SOME head
      end
        handle Subscript => NONE
  fun stringParser input =
      let
        val () = consumeInput := (! consumeInput ^ input)
        (* val () = consumeInput := input *) (* for test *)
      in
        parse get1string
      end
  fun mainParser () =
      let
        fun get1stdin () = T.input1 T.stdIn
      in
        parse get1stdin
      end
end (* struct *)
end (* local *)
