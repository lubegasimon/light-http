open Lwt.Infix
open Lwt_unix

let server_sock = socket PF_INET SOCK_STREAM 0

let start_server =
  let addr = ADDR_INET (Unix.inet_addr_loopback, 5000) in
  bind server_sock addr >>= fun () ->
  listen server_sock 10;
  Lwt.return_unit

let response_str =
  "HTTP/1.1 200 OK\r\n\
   Content-Length: 11\r\n\
   Connection: close\r\n\
   \r\n\
   Hello world"

let keep_listening =
  let rec accept_loop () =
    accept server_sock >>= fun (client_sock, _client_addr) ->
    let in_chan = Lwt_io.of_fd ~mode:Lwt_io.Input client_sock in
    let out_chan = Lwt_io.of_fd ~mode:Lwt_io.Output client_sock in
    let%lwt incoming_req = Lwt_io.read ~count:1024 in_chan in
    Lwt_io.printl incoming_req >>= fun () ->
    match state server_sock with
    | Opened -> Lwt_io.write out_chan response_str >>= fun () -> accept_loop ()
    | Closed ->
        Lwt_io.eprintl "Error: client connection is closed" >>= fun () ->
        Lwt.return ()
    | Aborted exn ->
        Lwt_io.eprintl ("An expected error: %s" ^ Printexc.to_string exn)
        >>= fun () -> Lwt.return ()
  in
  accept_loop ()

let () =
  let open Lwt.Infix in
  Lwt_main.run
    ( Lwt_io.printf "Server is running ...\n\n" >>= fun () ->
      start_server >>= fun () -> keep_listening )
