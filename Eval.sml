local
  structure A = Absyn
  structure V = Value
in
structure Eval
          : sig
            type env
            val emptyEnv : env
            val main : env -> A.exp -> V.value
          end
=
struct
  exception RuntimeError
  type env = string -> A.exp option
  val emptyEnv = fn _ => NONE
  fun addEnv new old =
      fn key => case new key of
                    x as SOME e => x
                  | NONE => old key
  fun isZeroPrim _ V.True = raise RuntimeError
    | isZeroPrim _ V.False = raise RuntimeError
    | isZeroPrim 0 V.Zero = true
    | isZeroPrim _ V.Zero = false
    | isZeroPrim n (V.Succ v) = isZeroPrim (n+1) v
    | isZeroPrim n (V.Pred v) = isZeroPrim (n-1) v
  fun isZero v = isZeroPrim 0 v
  (* env -> A.exp -> env * V.value *)
  fun main env exp =
      case exp of
          A.True => V.True
        | A.False => V.False
        | A.Zero => V.Zero
        | A.Succ n => V.Succ (main env n)
        | A.Pred n => V.Pred (main env n)
        | A.If (cond, e1, e2) =>
          (case main env cond of
               V.True => main env e1
             | V.False => main env e2
             | _ => raise RuntimeError)
        | A.IsZero n => if isZero (main env n) then V.True else V.False
end (* struct *)
end (* local *)
