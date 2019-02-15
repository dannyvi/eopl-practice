

(run 3 " let buffer = 0 in let producer = proc (n) letrec waiit (k) = if zero?(k)
then set buffer = n else begin print(-(k,-200)); (waiit -(k,1)) end in (waiit 5) in let consumer = proc (d) letrec busywait (k) = if zero?(buffer) then begin print(-(k,-100)); (busywait -(k,-1)) end else buffer in (busywait 0) in begin spawn(proc (d) (producer 44)); print(300); (consumer 86) end")


(run 100 " let buffer = 0
in let producer = proc (n)
 letrec
  waiit (k) = if zero?(k)
      then set buffer = n
      else begin
        print(-(k,-600));
        yield();
        (waiit -(k,1))
       end
  in (waiit 5)
 in let consumer = proc (d)
letrec busywait (k) = if zero?(buffer)
           then begin
             print(-(k,-100));
             (busywait -(k,-1))
             end
             else buffer
  in (busywait 0)
  in begin
     spawn(proc (d) (producer 44));
     print(300);
     (consumer 86)
  end
")

(run 200 "
let x = 0
in let mut = mutex()
in let incr_x = proc (id)
                proc (dummy)
                 begin
                  wait(mut);
                  set x = -(x,-1);
                  signal(mut)
                 end
in begin
   spawn((incr_x 100));
   spawn((incr_x 200));
   spawn((incr_x 300));
   print(x)
end
 ")



(run 30 "3")




(define-syntax-rule (pp  a b) (eopl:printf "~a ~a" a b))
