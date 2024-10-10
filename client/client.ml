open Http_

let () =
  let request =
    {
      Request.meth = `GET;
      uri = "/";
      headers = Some [ ("Host", "localhost:5000"); ("Connection", "close") ];
      version = `Http_1_1;
    }
  in
  let response = Lwt_main.run (send_http_request request) in
  let { Response.status_code; status_msg; headers; version; body } = response in
  Printf.printf "Received response : \n\n";
  Printf.printf "%s %s %s\r\n%s\r\n%s\n"
    (Version.to_string version)
    (Status.code_to_string status_code)
    (Status.msg_to_string status_msg)
    (Header.to_string (Option.get headers))
    (Option.get body)
