
local
  structure A = Absyn
  structure T = TextIO
in
structure Parser :
          sig
            val parser : (unit -> char option) -> unit -> A.exp
            val stringParser : string -> A.exp * string
            val mainParser : unit -> A.exp
          end
  =
struct
  fun curry f x y = f (x,y)
  fun uncurry f (x,y) = f x y

  datatype 'a result =
           Success of 'a * table ref | Miss | No
       and table =
           Table of {
           char : char result,
           token : string result,
           trueexp : A.exp result ref,
           falseexp : A.exp result ref,
           ifexp : A.exp result ref,
           zero : A.exp result ref,
           succ : A.exp result ref,
           pred : A.exp result ref,
           iszero : A.exp result ref
       } ref
  val initTable () = ref
                       {
                       (* TODO *)
                       }
  val table : table = initTable ()
  infix ++ **
  fun parser1 ++ parser2 get1 table =
      case parser1 get1 table of
        Success (exp1, nextTable) => Success (exp1, nextTable)
      | Miss => parser2 get1 table
      | No => Fail "Bug: ++"
  fun parser1 ** parser2 mixer get1 table =
      case parser1 get1 table of
        Success (exp1, nextTable) =>
        (case parser2 get1 nextTable of
           Success (exp2, resultTable) =>
           Success (mixer exp1 exp2, resultTable)
         |  => Miss
         | No => Fail "Bug: **")
      | Miss => Miss
      | No => Fail "Bug: **"
  fun parseCharPrim get1 table =
      case #char !table of
        x as Success (c, next) => x
      | Miss => raise Fail "parseCharPrim: Miss"
      | No => case get1 () of
                SOME c => Success (c, initTable ())
              | NONE => raise Fail "parseCharPrim: cannot get input character"
  fun parseChar c get1 table =
      case parseChar get1 table of
        x as Success (c', next) => if c = parseChar get1 table then x else Miss
      | Miss => raise Fail "parseChar: miss parseCharPrim"
      | No   => raise Fail "parseChar: miss parseCharPrim"
  fun parseTokenPrim nil nil get1 nextTable = Success ("", nextTable)
    | parseTokenPrim nil _   _    _ = Miss
    | parseTokenPrim _   nil get1 _ = Miss
    | parseTokenPrim (c1::rest1) (c2::rest2) get1 table =
      if c1 = c2 then           (* TODO *)
      (parseChar c ** parseTokenPrim rest) (curry op ^) get1 table
  fun parseTokenPrim get1 table =
  (* TODO *)
(*  fun parseToken str get1 table =
      let
        val charList = explode str
      in
        parseTokenPrim charList get1 table
      end *)
  fun parseToken str get1 table =
      case #token (! table) of
        x as Success (token, next) => if str = token then x else Miss
      | Miss => Miss
      | No => (parseTtokenPrim get1 table; parseToken str get1 table)
  fun parse get1 () =           (* TODO *)
  val consumeInput = ref ""
  fun get1 () =
      let
        val nowInput = !consumeInput
        val head = sub (nowInput, 0)
        val tail = String.substring (nowInput, 1, size nowInput - 1)
        val () = consumeInput := tail
      in
        SOME head
      end
        handle Subscript => NONE
  fun stringParser input =
      let
        val consumeInput := input
      in
        parser get1
      end
  fun mainParser () =
      let
        fun get1 () = T.input1 T.stdIn
      in
        parser get1
      end

type instream  <hidden>
type outstream  <hidden>
type vector = string
val canInput = fn : TextIO.instream * int -> int option
val closeIn = fn : TextIO.instream -> unit
val closeOut = fn : TextIO.outstream -> unit
val endOfStream = fn : TextIO.instream -> bool
val flushOut = fn : TextIO.outstream -> unit
val getInstream = fn : TextIO.instream -> TextIO.StreamIO.instream
val getOutstream = fn : TextIO.outstream -> TextIO.StreamIO.outstream
val getPosOut = fn : TextIO.outstream -> TextIO.StreamIO.out_pos
val input = fn : TextIO.instream -> string
val input1 = fn : TextIO.instream -> char option
val inputAll = fn : TextIO.instream -> string
val inputLine = fn : TextIO.instream -> string option
val inputN = fn : TextIO.instream * int -> string
val lookahead = fn : TextIO.instream -> char option
val mkInstream = fn : TextIO.StreamIO.instream -> TextIO.instream
val mkOutstream = fn : TextIO.StreamIO.outstream -> TextIO.outstream
val openAppend = fn : string -> TextIO.outstream
val openIn = fn : string -> TextIO.instream
val openOut = fn : string -> TextIO.outstream
val openString = fn : string -> TextIO.instream
val output = fn : TextIO.outstream * string -> unit
val output1 = fn : TextIO.outstream * char -> unit
val outputSubstr = fn : TextIO.outstream * Substring.substring -> unit
val print = fn : string -> unit
val scanStream = fn
  : ['a.
      ((TextIO.StreamIO.instream
          -> (char * TextIO.StreamIO.instream) option)
         -> TextIO.StreamIO.instream
              -> ('a * TextIO.StreamIO.instream) option)
        -> TextIO.instream -> 'a option]
val setInstream = fn : TextIO.instream * TextIO.StreamIO.instream -> unit
val setOutstream = fn : TextIO.outstream * TextIO.StreamIO.outstream -> unit
val setPosOut = fn : TextIO.outstream * TextIO.StreamIO.out_pos -> unit
val stdErr = _ : TextIO.outstream
val stdIn = _ : TextIO.instream
val stdOut = _ : TextIO.outstream

end

