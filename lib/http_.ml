(* http/1.1 protocol implementation *)

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
end

module Header = struct
  type t = string * string

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

  let msg_of_code = function
    | `Code 200 -> `OK
    | `Code 100 -> `Continue
    | `Code 101 -> `Switching_protocols
    | _ -> failwith "Unimplemented status code"
end

module Response = struct
  type response = {
    status_code : Status.code;
    status_msg : Status.msg;
    headers : Header.t list option;
    version : Version.t;
    body : string option;
  }

  let parse_headers rest =
    let rec aux acc = function
      | "\r" :: xs -> (List.rev acc, xs (* "\r" denotes end of header section *))
      (* Header section *)
      | x :: xs -> aux ((String.trim x |> Header.of_string) :: acc) xs
      | [] -> ([], [])
    in
    aux [] rest

  let get_status_line_parts status_line =
    let line_parts = String.(split_on_char ' ' (trim status_line)) in
    match line_parts with
    (* we ignore msg because we can compute it from the code *)
    | version :: code :: _msg ->
        let version = Version.of_string version in
        let code = `Code (int_of_string code) in
        let msg = Status.msg_of_code code in
        (version, code, msg)
    | _ -> failwith "Invalid status line"

  let of_string res =
    let lines = String.split_on_char '\n' res in
    match lines with
    | [] -> failwith "Empty response"
    | status_line :: rest ->
        let version, status_code, status_msg =
          get_status_line_parts status_line
        in
        let headers, body =
          let headers, body = parse_headers rest in
          let headers = match headers with [] -> None | xs -> Some xs in
          let body = Some (String.concat "" body) in
          (headers, body)
        in
        { version; status_code; status_msg; headers; body }
end
