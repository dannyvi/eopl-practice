(run
 "letrec
    removeall(n,s) =
      if null?(s)
      then emptylist
      else if number?(car(s))
           then if equal?(n,car(s))
                then (removeall n cdr(s))
                else cons(car(s), (removeall n cdr(s)))
           else cons((removeall n car(s)),
                     (removeall n cdr(s)))")
