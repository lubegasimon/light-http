(* http/1.1 protocol implementation *)

module Version : sig
  type t = [ `Http_1_1 | `Http_1_0 ]

  val to_string : t -> string
  val of_string : string -> t
end

module Method : sig
  type t = [ `GET | `HEAD ]

  val to_string : t -> string
  val of_string : string -> t
end

(** Generic headers (requests and responses) *)
module Header : sig
  type t = string * string
  type headers = t list

  val to_string : (string * string) list -> string
  val of_string : string -> headers
end

module Request : sig
  type t = {
    meth : Method.t;
    uri : string;
    headers : Header.headers;
    version : Version.t;
    body : string option;
  }

  val to_string : t -> string
end

module Status : sig
  type informational = [ `Continue | `Switching_protocols ]
  (** 1xx codes - Request received, continuing process *)

  (* 2xx success codes, The action was successfully received, understood, and accepted *)
  type success = [ `OK ]

  (* 4xx are client error codes *)
  type client_err = [ `Method_not_allowed ]

  (* 5xx are server error codes *)
  type server_err = [ `Not_implemented ]
  type msg = [ informational | success | client_err | server_err ]
  type code = [ `Code of int ]

  val code_to_string : code -> string
  val code_of_string : code -> string
  val msg_of_code : code -> msg
  val msg_to_string : msg -> string
end

module Response : sig
  type response = {
    status_code : Status.code;
    status_msg : Status.msg;
    headers : Header.headers;
    version : Version.t;
    body : string option;
  }

  val of_string : string -> response
end
