

(run 3 " let buffer = 0 in let producer = proc (n) letrec waiit (k) = if zero?(k)
then set buffer = n else begin print(-(k,-200)); (waiit -(k,1)) end in (waiit 5) in let consumer = proc (d) letrec busywait (k) = if zero?(buffer) then begin print(-(k,-100)); (busywait -(k,-1)) end else buffer in (busywait 0) in begin spawn(proc (d) (producer 44)); print(300); (consumer 86) end")


(run 1 " let buffer = 0
in let producer = proc (n)
 letrec
  waiit (k) = if zero?(k)
      then set buffer = n
      else begin
        print(-(k,-600));

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

(run 10 "
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
   kill(1);
   spawn((incr_x 300));
   spawn((incr_x 300));
   spawn((incr_x 300));
   spawn((incr_x 600));
   print(x)
end
 ")

;; exer5.51

(run 20 " let buffer = 0 in
let mut = mutex()
in let producer = proc (n)
 letrec
  waiit (k) = if zero?(k)
      then set buffer = n
      else begin
        print(-(k,-600));
        (waiit -(k,1))
       end
  in  begin wait(mut); (waiit 5); signal(mut) end
 in let consumer = proc (d)
letrec busywait (k) = if zero?(buffer)
           then begin
             print(-(k,-100));
             (busywait -(k,-1))
             end
             else begin print(buffer); buffer end
  in begin wait(mut); (busywait 0); signal(mut) end
  in begin
     spawn(proc (d) (producer 44));
     print(300);
     spawn(proc (d) (consumer 86))
  end
")

(run 20 " let buffer = 0 in
let mut = mutex()
in let producer = proc (n)
 letrec
  waiit (k) = if zero?(k)
      then begin set buffer = n; buffer end
      else begin
        print(-(k,-600));
        (waiit -(k,1))
       end
  in  begin wait(mut); (waiit 8); signal(mut) end
 in let consumer = proc (d)
     begin wait(mut); print(buffer); buffer; signal(mut) end

  in begin
     spawn(proc (d) (producer 44));
     print(300);
     spawn(proc (d) (consumer 86))
  end
")

(run  20
"let buffer = 0
   in let mut = mutex()
      in let producer = proc (n)
           letrec wait1(k) = if zero?(k)
             then
               begin set buffer = n;
                    signal(mut)
               end
             else begin print(-(k, -200));
                       (wait1 -(k, 1))
                 end
           in (wait1 5)
         in let consumer = proc (d)
                             begin wait(mut);
                                   buffer
                             end
            in begin wait(mut);
                     spawn(proc (d) (producer 44));
                     print(300);
                     (consumer 86)
               end")


(run 30 "3")

(run 1 " let buffer = 10
in let consumer = proc (d)
letrec busywait (k) = if zero?(buffer)
           then begin
             print(-(k,-100));
             (busywait -(k,-1))
             end
             else buffer
  in (busywait 0)
  in begin
     
     print(300);
     (consumer 86)
  end
")


(run 10
"letrec infloop (k) = if zero?(k) then 0 else begin print(k); (infloop -(k,1)) end
 in letrec stopper (n) = if zero?(n) then  begin kill(1); 1 end
        else
          begin
            print(-(n,200));

            (stopper -(n,1))
          end
  in begin
      spawn(proc (d) (infloop 1000))  ;
      print(200);
      (stopper 100)
      %spawn(proc (d) (stopper 100))
     end
      ")

(run 10 "let r = proc (k) let a = recv() in if zero?(a)
                            then print(100) else print(80)
           in letrec s (n) = if zero?(n) then send(1, 1) else begin print(n);
(s -(n,1)) end
            in begin
                spawn(proc (d) (r 99));
               (s 100)
end  ")

(run 10 "let r = proc (k)  if zero?(recv())
                            then print(100) else print(80)
           in letrec s (n) = if zero?(n) then send(1, 33) else begin print(n);
(s -(n,1)) end
in begin
spawn(proc (d) (r 99));
(s 100)
end  ")

(define-syntax-rule (pp  a b) (eopl:printf "~a ~a" a b))
