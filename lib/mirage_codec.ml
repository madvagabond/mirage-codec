
module type Encoder = sig

  type output 
  val encode: output -> Cstruct.t
end





type 'a decode_state = Partial | Done of ('a * Cstruct.t)
type 'a out = ('a * Cstruct.t) option




type ('a, 'b) decode_result =  ('a decode_state, 'b) result



module type Decoder = sig

  type input
  type error
    
  val decode: Cstruct.t -> (input, error) decode_result
end





(*
module type TRANSPORT = struct

  type t
  type message 

  val frame: t -> result 
  val read: 
end

*)




module type Codec = sig
  include Decoder
  include Encoder
end





module type Transport = sig

  type t

  
  type input
  type output

  type error
  type write_error

  
  type or_eof =
    | Data of input
    | Eof



  val read: t -> (or_eof, error) result Lwt.t
  val write: t -> output -> (unit, write_error) result Lwt.t

  val writev: t -> output list -> (unit, write_error) result Lwt.t

  val close: t -> unit Lwt.t
  
  
  
end





module Make (F: Mirage_flow_lwt.S) (C: Codec): Transport = struct

  open Lwt.Infix


  type input = C.input
  type output = C.output

  
  type write_error = F.write_error
  
  
  type t = {
    flow: F.flow;
    mutable buffer: Cstruct.t;
  }




  type error =
    | Flow_error of F.error
    | Codec_error of C.error
  


  type or_eof =
    | Data of C.input
    | Eof




  
  
  
  
  let read t =


    
    let handle_success res =
      let (input, rest) = res in 
      let _ = t.buffer <- rest in

      
      Lwt.return (Ok (Data input) )
    in


    
    let rec aux () =


      let perform_read () =
        F.read t.flow >>= function

        | Ok (`Eof) -> Lwt.return (Ok Eof)

        | Ok (`Data b) ->
          let _ = t.buffer <- Cstruct.append t.buffer b in
          aux () 

        | Error e ->
          Lwt.return (Error (Flow_error e) )
      in



    

      if Cstruct.len t.buffer > 0 then
        C.decode t.buffer |> function
        | Ok (Done res) -> handle_success res
        | Ok Partial -> perform_read ()
        | Error e -> Lwt.return ( Error (Codec_error e)  )


      else
        perform_read ()

    in

    aux ()


  
  



  let write t out =
    let buf = C.encode out in 
    F.write t.flow buf





  let writev t out =
    let out_v = List.map C.encode out in 
    F.writev t.flow out_v



  
  let close t =
    F.close t.flow 


  
  
  
end 
