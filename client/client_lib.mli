open Lib.Http
open Lwt_unix

val connect_to_server : file_descr Lwt.t
val send_request : file_descr -> Request.t -> file_perm Lwt.t
val send_http_request : Request.t -> Response.response Lwt.t
