open Lib.Http

val send_http_request : Request.t -> Response.response Lwt.t
