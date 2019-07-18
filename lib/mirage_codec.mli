



module type Encoder = sig

  type output 
  val encode: output -> Cstruct.t
end





type 'a decode_state = Partial | Done of ('a * Cstruct.t)

type ('a, 'b) decode_result =  ('a decode_state, 'b) result



module type Decoder = sig

  type input
  type error
    
  val decode: Cstruct.t -> (input, error) decode_result
end




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

module Make: functor (F: Mirage_flow_lwt.S) (C:  Codec) -> sig
  
  type t

  
  type input = C.input
  type output =  C.output 

  
  type or_eof =
    | Data of input
    | Eof


  
  type write_error = F.write_error


  type error =
    | Flow_error of F.error
    | Codec_error of C.error
  


  val read: t -> (or_eof, error) result Lwt.t
  val write: t -> output -> (unit, write_error) result Lwt.t

  val writev: t -> output list -> (unit, write_error) result Lwt.t

  val close: t -> unit Lwt.t
  val make: F.flow -> t


  
  
  
end
