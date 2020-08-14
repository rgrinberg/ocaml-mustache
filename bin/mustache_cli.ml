let apply_mustache json_data template_data =
  let env = Ezjsonm.from_string json_data
  and tmpl = Mustache.of_string template_data
  in
  Mustache.render tmpl env 

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.to_string s)

let run ?(prepend_json=ref false) json_filename template_filename =
  let j = load_file json_filename
  and t = load_file template_filename in
  let result = apply_mustache j t in
  (match prepend_json.contents with
   | true -> [j; result] |> String.concat ""
   | false -> result)
  |> print_endline

let () =
  let usage =  ["Usage: mustache-cli [option] json_filename template_filename";
                "  options:"] |> String.concat "\n" in
  let prepend_json = ref false in
  let filenames:string list ref = ref [] in
  let opts =  [("-p", Arg.Set prepend_json, "prepend json to resulting output");] in
  Arg.parse opts (fun s -> filenames.contents <- filenames.contents @ [s]) usage;
  match !filenames with
  | [ json_filename ; template_filename ]
    -> run ~prepend_json json_filename template_filename
  | _ -> Arg.usage opts usage;
