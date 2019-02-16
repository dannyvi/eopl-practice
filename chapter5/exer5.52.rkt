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

(run 20
"let x = 0
   in let mut = mutex()
      in let incr_x = proc (id)
                        let mut1 = mutex()
                        in begin wait(mut1);
                                 spawn(proc (dummy)
                                         begin wait(mut);
                                               set x = -(x, -1);
                                               signal(mut);
                                               signal(mut1)
                                         end);
                                 mut1
                           end
         in let mut1 = (incr_x 100)
            in let mut2 = (incr_x 200)
               in let mut3 = (incr_x 300)
                  in begin wait(mut1);
                           wait(mut2);
                           wait(mut3);
                           x
                     end")
