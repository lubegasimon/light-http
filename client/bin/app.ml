open Http
open Client

let () =
  let request =
    {
      Request.meth = `GET;
      uri = "/";
      headers = [ ("Host", "localhost:5000"); ("Connection", "close") ];
      version = `Http_1_1;
      body = Some "Hello there";
    }
  in
  let res = Lwt_main.run (send_http_request request) in
  let body = match res.body with None -> "" | Some body -> body in
  Printf.printf "Received response : \n\n";
  Printf.printf "%s %s %s\r\n%s\r\n%s\n"
    (Version.to_string res.version)
    (Status.code_to_string res.status_code)
    (Status.msg_to_string res.status_msg)
    (Header.to_string res.headers)
    body
