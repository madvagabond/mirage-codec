
open Mirage_codec
open Lwt.Infix
       

module TestCodec = struct

  type input = string
  type output = string




  type error = unit

  

  let encode out =
    
    let b = Cstruct.create 2 in
    let _ = Cstruct.BE.set_uint16 b 0 (String.length out) in

    Cstruct.append b (Cstruct.of_string out)


  let partial = Ok (Partial)

  

  let decode input =

    let handle_body len =
      let buf = Cstruct.shift input 2 in 

      if Cstruct.len buf >= len then
        let (consumed, rest) = Cstruct.split buf len in
        let s = Cstruct.to_string consumed in
        let res = Done (s, rest) in
        Ok res

      else
        partial

    in
    
    
    if Cstruct.len input >= 2 then
      Cstruct.BE.get_uint16 input 0 |> handle_body

    else
      partial
  
      
end




module TestTransport = Mirage_codec.Make(Mirage_flow_unix.Fd) (TestCodec)




let (tx, rx) =
  let (x, y) = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM) 0 in
  TestTransport.make x, TestTransport.make y



let read_n n =
  let open TestTransport in 
  let rec aux v =
    if List.length v < n then 
      TestTransport.read rx >>= function

      | Ok (Data x) -> aux (v @ [x])
      | Ok (Eof) -> Lwt.return v
      | Error _ ->  Lwt.fail_with "Error"

    else
      Lwt.return v

  in
  aux []




let test_transport _s () =
  let open Alcotest in

  
  let v = ["hello"; "how are you"; "I am shit nanana"; "Suicide Boys"] in
  let len = List.length v in

  TestTransport.writev tx v >>= fun _ ->
  read_n len >|= fun got ->

  Alcotest.check
    (list string)
    "Received all sent payloads in original form"
    v
    got



let suite = [
  Alcotest_lwt.test_case "Testing Transport" `Quick test_transport
]





let _ = Alcotest.run "Transport Suite" ["", suite]








