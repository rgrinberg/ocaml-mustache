type format = [`Json | `Yaml]

exception EnvParseError of string

let env_of_json (env : string) : Mustache.Env.t =
  try
    Ezjsonm.from_string env
  with Ezjsonm.Parse_error (_, msg) ->
    raise (EnvParseError msg)

let env_of_yaml (env : string) : Mustache.Env.t =
  match Yaml.of_string env with
  | Ok (#Mustache.Env.t as env) ->
      env
  | Ok _ ->
      raise (EnvParseError "YAML document root must be a list or map")
  | Error (`Msg msg) ->
      raise (EnvParseError msg)

let env_of (format : format) (env : string): Mustache.Env.t =
  match format with
  | `Json -> env_of_json env
  | `Yaml -> env_of_yaml env

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.to_string s)

type args = {
  format  : format;
  strict  : bool;
  envname : string;
  mstname : string;
}

let usage =
  "Usage: mustache [-f json|yaml] [-s] filename.{json,yaml} filename.mustache\n"

let arg_format (format : string) =
  match String.lowercase_ascii format with
  | "json" -> `Json
  | "yaml" -> `Yaml
  | _      -> raise (Arg.Bad (Printf.sprintf "invalid format: %s" format))

let () =
  let args =
    let format = ref None in
    let strict = ref false in
    let extra  = ref [] in

    let specs = [
      ("-f", Arg.String (fun s -> format := Some (arg_format s)),
       ": set input format for the environment (= [json|yaml])");
      ("-s", Arg.Set strict,
       ": interpret the template in strict mode");
    ] in
      
    try
      begin try
        Arg.parse_argv
          Sys.argv specs (fun x -> extra := x :: !extra) usage;
      with 
      | Arg.Bad msg ->
          let msg =
            if   String.contains msg '\n'
            then String.sub msg 0 (String.index msg '\n')
            else msg in

          let msg =
            if String.contains msg ':' then
              let index = String.index msg ':' in
              String.sub msg (index+1) (String.length msg - (index+1))
            else msg in

          raise (Arg.Bad (String.trim msg))
      end;
  
      let envname, mstname =
        match !extra with
        | [mstname; envname] -> (envname, mstname)
        | _ -> raise (Arg.Bad "the program expects exactly 2 mandatory arguments") in
  
      let format =
        match !format with
        | Some format -> format
        | None -> begin
            match String.lowercase_ascii (Filename.extension envname) with
            | ".json" -> `Json
            | ".yml" | ".yaml" -> `Yaml
            | _ -> raise (Arg.Bad "cannot guess environment format from extension")
          end in

      let strict = !strict in

      { format; strict; envname; mstname; }

    with
    | Arg.Bad msg ->
      Format.eprintf "%s: %s@.@.%s@."
        (Filename.basename Sys.argv.(0)) msg (Arg.usage_string specs usage);
      exit 1

    | Arg.Help _ ->
      Format.eprintf "%s@." (Arg.usage_string specs usage);
      exit 1
  in

  try
    let env  = env_of args.format (load_file args.envname) in
    let tmpl = Mustache.of_string (load_file args.mstname) in

    Mustache.render ~strict:args.strict tmpl env |> print_endline

  with Mustache.Parse_error err ->
    Format.eprintf "%a@." Mustache.pp_error err;
    exit 1
