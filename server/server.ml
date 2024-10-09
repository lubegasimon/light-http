open Http_

let start_simple_server () =
  let open Lwt.Infix in
  let open Lwt_unix in
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET (Unix.inet_addr_loopback, 5000) in
  bind server_sock addr >>= fun () ->
  listen server_sock 10;
  let rec accept_loop () =
    accept server_sock >>= fun (client_sock, _client_addr) ->
    let in_chan = Lwt_io.of_fd ~mode:Lwt_io.Input client_sock in
    let out_chan = Lwt_io.of_fd ~mode:Lwt_io.Output client_sock in
    Lwt_io.read ~count:1024 in_chan >>= fun request_str ->
    Printf.printf "Received request:\n%s\n" request_str;
    let response_str =
      "HTTP/1.1 200 OK\r\n\
       Content-Length: 11\r\n\
       Connection: close\r\n\
       \r\n\
       Hello world"
    in
    Lwt_io.write out_chan response_str >>= fun () -> accept_loop ()
  in
  accept_loop ()

let () =
  let server_thread = start_simple_server () in
  let client_thread =
    let open Lwt.Infix in
    let request =
      {
        Request.meth = `GET;
        uri = "/";
        headers = Some [ ("Host", "localhost:5000"); ("Connection", "close") ];
        version = `Http_1_1;
      }
    in
    Lwt_unix.sleep 1.0 >>= fun () ->
    (* Ensure server starts first *)
    send_http_request request >>= fun _ -> Lwt.return_unit
  in
  Lwt_main.run (Lwt.join [ server_thread; client_thread ])
