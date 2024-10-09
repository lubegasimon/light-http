(* http/1.1 protocol implementation *)
(* open Conduit_lwt_unix *)
open Lwt

module Version = struct
  type t = [ `Http_1_1 | `Http_1_0 ]

  let to_string = function `Http_1_0 -> "HTTP/1.0" | `Http_1_1 -> "HTTP/1.1"

  let of_string = function
    | "HTTP/1.0" -> `Http_1_0
    | "HTTP/1.1" -> `Http_1_1
    | _ -> failwith "Version is unimplemented"
end

module Meth = struct
  type t = [ `GET | `HEAD ]

  let to_string = function `GET -> "GET" | `HEAD -> "HEAD"

  let _of_string = function
    | "GET" -> `GET
    | "HEAD" -> `HEAD
    | _ -> failwith "Method is unimplemented"
end

module Header = struct
  type t = string * string
  (* type headers = t list *)

  (* TODO: type check headers *)
  (* TODO: Perf: string concat [^] is inefficient *)
  let to_string headers =
    List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers |> String.concat ""

  let of_string header_section : t =
    let header = String.split_on_char ':' header_section in
    match header with
    | [ field_name; field_value ] -> (field_name, field_value)
    | _ -> failwith "Header is unimplemented"
end

module Request = struct
  type t = {
    meth : Meth.t;
    uri : string;
    headers : Header.t list option;
    version : Version.t;
  }

  let to_string req =
    let { meth; uri; version; headers } = req in
    let req_line =
      Meth.to_string meth ^ " " ^ uri ^ " " ^ Version.to_string version ^ "\r\n"
    in
    let header =
      match headers with
      | Some headers -> Header.to_string headers ^ "\r\n"
      | None -> ""
    in
    req_line ^ header
end

module Status = struct
  (* 1xx codes - Request received, continuing process *)
  type informational = [ `Continue | `Switching_protocols ]

  (* 2xx success codes, The action was successfully received, understood, and accepted *)
  type success = [ `OK ]
  type msg = [ informational | success ]
  type code = [ `Code of int ]

  let code_to_string = function `Code code -> string_of_int code

  let msg_to_string = function
    | `OK -> "OK"
    | `Continue -> "Continue"
    | `Switching_protocols -> "Switching_protocols"

  let _msg_of_string = function
    | "OK" -> `OK
    | "Continue" -> `Continue
    | "Switching_protocols" -> `Switching_protocols
    | _ -> failwith "Unknown status message"
end

module Response = struct
  type response = {
    status_code : Status.code;
    status_msg : Status.msg;
    headers : Header.t list option;
    version : Version.t;
    body : string option;
  }

  (** [parse_req req] parses request [req] and constructs response *)
  let _parse_req (req : Request.t) =
    let { Request.meth; version; _ } = req in
    if Meth.to_string meth <> "GET" then failwith "Expected a GET method"
    else
      let status_code = `Code 200 in
      let status_msg = `OK in
      let headers = None in
      { status_code; status_msg; headers; version; body = Some "Hello world" }

  let of_string res =
    let lines = String.split_on_char '\n' res in
    match lines with
    | [] -> failwith "Empty response"
    | status_line :: rest ->
        let line_components =
          String.trim status_line |> String.split_on_char ' '
        in
        let version, code =
          match line_components with
          (* we ignore msg because we can compute it from the code *)
          | version :: code :: _msg -> (version, code)
          | _ -> failwith "Invalid status line"
        in
        (* parse headers *)
        let parse_headers rest =
          let rec aux acc = function
            | "\r" :: xs ->
                (List.rev acc, xs (* "\r" denotes end of header section *))
            (* Header section *)
            | x :: xs -> aux ((String.trim x |> Header.of_string) :: acc) xs
            | [] -> ([], [])
          in
          aux [] rest
        in
        let headers, body = parse_headers rest in
        let headers =
          match headers with [] -> None | headers -> Some headers
        in
        let version = Version.of_string version in
        let status_code = `Code (int_of_string code) in
        let status_msg =
          match status_code with
          | `Code 200 -> `OK
          | `Code 100 -> `Continue
          | `Code 101 -> `Switching_protocols
          | _ -> failwith "Unimplemented status code"
        in
        let body = Some (String.concat "" body) in
        { version; status_code; status_msg; headers; body }

  (** [to_string res] is http response string *)
  let _to_string res =
    let { status_code; status_msg; version; headers; body } = res in
    let open Status in
    let headers =
      match headers with
      | Some headers -> Header.to_string headers ^ "\r\n"
      | None -> ""
    in
    let body = match body with Some body -> body | None -> "" in
    let status_line =
      Version.to_string version ^ code_to_string status_code ^ " "
      ^ msg_to_string status_msg ^ "\r\n"
    in
    status_line ^ headers ^ body
end

let connect_to_server =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET (Unix.inet_addr_loopback, 5000) in
  connect sock addr >>= fun _ -> Lwt.return sock

let send_request sock req =
  let req_str = Request.to_string req in
  let req_bytes = Bytes.of_string req_str in
  Lwt_unix.write sock req_bytes 0 (Bytes.length req_bytes)

let receive_response sock =
  let buf_size = 4096 in
  let buf = Bytes.create buf_size in
  let%lwt bytes_read = Lwt_unix.read sock buf 0 buf_size in
  if bytes_read == 0 then Lwt.return (Bytes.to_string Bytes.empty)
  else
    (* accumulate whatever you read *)
    let contents = Bytes.sub_string buf 0 bytes_read in
    Lwt.return contents

let send_http_request request =
  connect_to_server >>= fun sock ->
  send_request sock request >>= fun _ ->
  receive_response sock >>= fun res_str ->
  Lwt_unix.close sock >>= fun _ -> Lwt.return (Response.of_string res_str)
