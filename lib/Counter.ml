class counter (initial_value : int) =
  object (_self)
    val mutable count = initial_value (* Initialize with constructor value *)
    method get_count = count (* Getter method *)
    method increment = count <- count + 1 (* Mutate the count *)

    method increment_by n =
      count <- count + n (* Increment by specific amount *)

    method reset = count <- initial_value (* Reset to initial value *)
  end

(* Create a new instance starting at 10 *)
let my_counter = new counter 10

(* Use the methods *)
let () =
  Printf.printf "Initial count: %d\n" my_counter#get_count;

  my_counter#increment;
  (* Increment by 1 *)
  Printf.printf "After increment: %d\n" my_counter#get_count;

  my_counter#increment_by 5;
  (* Increment by 5 *)
  Printf.printf "After adding 5: %d\n" my_counter#get_count;

  my_counter#reset;
  (* Reset to initial value (10) *)
  Printf.printf "After reset: %d\n" my_counter#get_count
