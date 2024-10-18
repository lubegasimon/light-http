open Lwt.Infix
open Lwt_unix
open Http

let connect_to_server =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET (Unix.inet_addr_loopback, 5000) in
  let ( let* ) = Lwt.bind in
  try%lwt connect sock addr >>= fun _ -> Lwt.return sock with
  | Unix.Unix_error (Unix.ECONNREFUSED, "connect", _) ->
      let* () = Lwt_io.eprintl "Error: failed to connect to server" in
      Lwt_io.flush_all () >>= fun () -> Lwt.return sock
  | Unix.Unix_error (err_code, func, param) ->
      let* () =
        Lwt_io.eprintf "Error: %s in %s(%s) %!"
          (Unix.error_message err_code)
          func param
      in
      Lwt_io.flush_all () >>= fun () -> Lwt.return sock
  | exn ->
      let* () =
        Lwt_io.eprintl ("An expected error: %!" ^ Printexc.to_string exn)
      in
      Lwt_io.flush_all () >>= fun () -> Lwt.return sock

let send_request sock req =
  let req_str = Request.to_string req in
  let req_bytes = Bytes.of_string req_str in
  write sock req_bytes 0 (Bytes.length req_bytes)

let receive_response sock =
  let buf_size = 4096 in
  let buf = Bytes.create buf_size in
  let%lwt bytes_read = read sock buf 0 buf_size in
  if bytes_read == 0 then Lwt.return (Bytes.to_string Bytes.empty)
  else
    let contents = Bytes.sub_string buf 0 bytes_read in
    Lwt.return contents

let send_http_request request =
  connect_to_server >>= fun sock ->
  send_request sock request >>= fun _ ->
  receive_response sock >>= fun res_str ->
  Lwt.return (Response.of_string res_str)
