#use "topfind" ;;

#require "lwt" ;;

from_generator : (('a -> ?) -> unit) -> 'a enum

(* loop : string -> 'a Lwt.t *)
let rec loop s = begin
    Lwt_unix.sleep 1. >>= fun () -> Format.printf "Hello %s@." s ;
    loop s
end

let thread1 = loop "A"
let thread2 = loop "B"
let (a, _) = Lwt.wait () in Lwt.run a ;;

val Lwt.bind : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val Lwt_unix.sleep : float -> unit Lwt.t

(* val count : (int -> 'a) -> unit *)
let count sender =
    for i = 1 to 10 do
        sender i ; print_string "."
    done
;;

let waiter, wakener = Lwt.wait () ;;
Lwt.state waiter ;;
Lwt.wakeup wakener 42 ;;
Lwt.state waiter ;;

On a un unit Lwt.u, on le réveille avec Lwt.wakeup it () et on obtient la valeur suivante, et le Lwt.u suivant…

Il faut une fonction à binder du type…

() -> ('a * unit Lwt.u) Lwt.t

On a un truc dans l'autre sens ?

let plou () = begin
    let (waiter, wakener) = Lwt.wait () in
    let wai = ref waiter in
    let bla x = begin
        !wai >>= fun () -> begin
            let (waiter, wakener) = Lwt.wait () in 
            wai := waiter ;
            Lwt.return (x, wakener)
        end
    end in
    (wakener, bla)
end ;;

let (pla, pli) = plou () ;;
