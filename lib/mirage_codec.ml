
module type Encoder = sig

  type t 
  val encode: t -> Cstruct.t
end





type 'a out = ('a * Cstruct.t) option
type ('a, 'b) decode_result =  ('a out, 'b) result




module type Decoder = sig

  type t
  type error
    
  val decode: Cstruct.t -> (t, error) decode_result
end




(*
module type TRANSPORT = struct

  type t
  type message 

  val frame: t -> result 
  val read: 
end

*)



module Make (D: Decoder) (E: Encoder)  (F: Mirage_flow_lwt.S) = struct

  open Lwt.Infix
         
  type tx = E.t 
  type rx = D.t 

  
  type t = {
    flow: F.flow;
    mutable buffer: Cstruct.t;
  }



  
  
  let read t =

    let handle_success res =
      let (input, rest) = res in 
      let _ = t.buffer <- rest in
      Lwt.return (Ok input)
    in


    let perform_read () =
      F.read t.flow >|= function

      | Ok (`Eof) -> Lwt.return (Ok `Eof)

      | Ok (`Data b) ->
        let _ = t.buffer <- Cstruct.append t.buffer b in
        Lwt.return ( Ok `Done )
        
      | Error e ->
        Lwt.return (Error e)
    in



    
    
    let rec aux () =

      if Cstruct.len t.buffer > 0 then
        D.decode t.buffer |> function
        | Ok (Some res) -> handle_success res
        | Ok None -> perform_read () >>= fun _ -> aux ()
        | Error e -> Lwt.return (Error e)


      else
        perform_read () >>= fun _ -> aux ()


    in

    aux ()


  
  
end 
