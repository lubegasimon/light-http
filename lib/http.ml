(* http/1.1 protocol implementation *)

exception Invalid_method
exception Invalid_first_line
exception Version_not_implemented
exception Invalid_http_string

module Version = struct
  type t = [ `Http_1_1 | `Http_1_0 ]

  let to_string = function `Http_1_0 -> "HTTP/1.0" | `Http_1_1 -> "HTTP/1.1"

  let of_string = function
    | "HTTP/1.0" -> `Http_1_0
    | "HTTP/1.1" -> `Http_1_1
    | _ -> raise Version_not_implemented
end

module Meth = struct
  type t = [ `GET | `HEAD ]

  let to_string = function `GET -> "GET" | `HEAD -> "HEAD"

  let of_string = function
    | "GET" -> `GET
    | "HEAD" -> `HEAD
    | _ -> raise Invalid_method
end

module Header = struct
  type t = string * string
  type headers = t list

  let to_string headers =
    List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers |> String.concat ""

  let of_string header_section : headers =
    let header = String.split_on_char ':' header_section in
    match header with
    | [ field_name; field_value ] -> [ (field_name, field_value) ]
    | _ -> []
end

(** [parse_http_str x] is first line of HTTP string [x] *)
let parse_http_str http_str =
  let lines = String.(split_on_char '\n' (trim http_str)) in
  match lines with
  | fst_ln :: rest -> (
      let fst_ln_parts = String.(split_on_char ' ' (trim fst_ln)) in
      match fst_ln_parts with
      | fst :: snd :: lst :: _ -> ((fst, snd, lst), rest)
      | _ -> raise Invalid_first_line)
  | [] -> raise Invalid_http_string

(** [parse_others x] is header section and body of parts [x] of HTTP string *)
let parse_others =
  let rec aux acc = function
    | "\r" :: body ->
        ( List.(rev (flatten acc)),
          Some (String.concat "" body) (* "\r" denotes end of header section *)
        )
    (* Header section *)
    | x :: xs -> aux ((String.trim x |> Header.of_string) :: acc) xs
    | [] -> ([], Some "")
  in
  aux []

module Request = struct
  type t = {
    meth : Meth.t;
    uri : string;
    headers : Header.headers;
    version : Version.t;
    body : string option;
  }

  let to_string req =
    let req_ln =
      Meth.to_string req.meth ^ " " ^ req.uri ^ " "
      ^ Version.to_string req.version
      ^ "\r\n"
    in
    let header =
      match req.headers with
      | [] -> "\r\n"
      | headers -> Header.to_string headers ^ "\r\n"
    in

    let body = match req.body with Some body -> body | None -> "" in
    req_ln ^ header ^ body

  let of_string http_req_str =
    let (meth, uri, version), rest = parse_http_str http_req_str in
    let headers, body = parse_others rest in
    {
      meth = Meth.of_string meth;
      uri;
      version = Version.of_string version;
      headers;
      body;
    }
end

module Status = struct
  (* 1xx codes - Request received, continuing process *)
  type informational = [ `Continue | `Switching_protocols ]

  (* 2xx success codes, The action was successfully received, understood, and accepted *)
  type success = [ `OK ]

  (* 4xx are client error codes *)
  type client_err = [ `Method_not_allowed ]

  (* 5xx are server error codes *)
  type server_err = [ `Not_implemented ]
  type msg = [ informational | success | client_err | server_err ]
  type code = [ `Code of int ]

  let code_to_string = function `Code code -> string_of_int code
  let code_of_string = function `Code code -> string_of_int code

  let msg_to_string = function
    | `OK -> "OK"
    | `Continue -> "Continue"
    | `Switching_protocols -> "Switching Protocols"
    | `Method_not_allowed -> "Method Not Allowed"
    | `Not_implemented -> "Not Implemented"

  let msg_of_code = function
    | `Code 200 -> `OK
    | `Code 100 -> `Continue
    | `Code 101 -> `Switching_protocols
    | `Code 405 -> `Method_not_allowed
    | `Code 501 | _ -> `Not_implemented
end

module Response = struct
  type response = {
    status_code : Status.code;
    status_msg : Status.msg;
    headers : Header.headers;
    version : Version.t;
    body : string option;
  }

  let of_string http_res_str =
    let (version, code, _msg), rest = parse_http_str http_res_str in
    let headers, body = parse_others rest in
    let status_code = `Code (int_of_string code) in
    {
      version = Version.of_string version;
      status_code;
      status_msg = Status.msg_of_code status_code;
      headers;
      body;
    }
end
