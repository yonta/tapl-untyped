structure AbsynFormatter =
struct
  local
      structure A = Absyn
      structure SF = SMLFormat
  in
  fun exp2string exp = SF.prettyPrint [SF.Columns 80] (A.format_exp exp)
  end
end
