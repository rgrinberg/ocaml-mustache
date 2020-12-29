(*{{{ The MIT License (MIT)

   Copyright (c) 2015 Rudi Grinberg

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to
   deal in the Software without restriction, including without limitation the
   rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
   sell copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
   IN THE SOFTWARE.  }}}*)
{
  open Lexing
  open Mustache_parser

  exception Error of string

  let tok_arg lexbuf f =
    let start_p = lexbuf.Lexing.lex_start_p in
    let x = f lexbuf in
    lexbuf.Lexing.lex_start_p <- start_p;
    x

  let lex_tag lexbuf space ident tag_end =
    tok_arg lexbuf (fun lexbuf ->
      let () = space lexbuf in
      let name = ident lexbuf in
      let () = space lexbuf in
      let () = tag_end lexbuf in
      name
    )

  let split_ident ident =
    if ident = "." then []
    else String.split_on_char '.' ident

  let check_mustaches ~expected ~lexed =
    if expected <> lexed then
      raise (Error (Printf.sprintf "'%s' expected" expected))
}

let blank = [' ' '\t']*
let newline = ('\n' | "\r\n")
let raw = [^ '{' '}' '\n']*
let id = ['a'-'z' 'A'-'Z' '-' '_' '/'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '/']*
let ident = ('.' | id ('.' id)*)

(* The grammar of partials is very relaxed compared to normal
   identifiers: we want to allow dots anywhere to express relative
   paths such as ../foo (this is consistent with other implementations
   such as the 'mustache' binary provided by the Ruby implementation),
   and in general we don't know what is going to be used, given that
   partials are controlled programmatically.

   We forbid spaces, to ensure that the behavior of trimming spaces
   around the partial name is consistent with the other tag, and we
   forbid newlines and mustaches to avoid simple delimiter mistakes
   ({{> foo } ... {{bar}}) being parsed as valid partial names.

   (Note: if one wishes to interpret partials using lambdas placed
   within the data (foo.bar interpreted as looking up 'foo' then 'bar'
   in the input data and hoping to find a user-decided representation
   of a function, it is of course possible to restrict the valid names
   and split on dots on the user side.) *)
let partial_name = [^ ' ' '\t' '\n' '{' '}']*

rule space = parse
  | blank newline { new_line lexbuf; space lexbuf }
  | blank { () }

and ident = parse
  | ident { lexeme lexbuf }
  | ""    { raise (Error "ident expected") }

and partial_name = parse
  | partial_name { lexeme lexbuf }

and end_on expected = parse
  | ("}}" | "}}}" | "") as lexed { check_mustaches ~expected ~lexed }

and comment acc = parse
  | "}}"        { String.concat "" (List.rev acc) }
  | raw newline { new_line lexbuf; comment ((lexeme lexbuf) :: acc) lexbuf }
  | raw         { comment ((lexeme lexbuf) :: acc) lexbuf }
  | ['{' '}']   { comment ((lexeme lexbuf) :: acc) lexbuf }
  | eof         { raise (Error "non-terminated comment") }

and mustache = parse
  | "{{"         { ESCAPE (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{{"        { UNESCAPE (lex_tag lexbuf space ident (end_on "}}}") |> split_ident) }
  | "{{&"        { UNESCAPE (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{#"        { OPEN_SECTION (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{^"        { OPEN_INVERTED_SECTION (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{/"        { CLOSE_SECTION (lex_tag lexbuf space ident (end_on "}}") |> split_ident) }
  | "{{>"        { PARTIAL (0, lex_tag lexbuf space partial_name (end_on "}}")) }
  | "{{!"        { COMMENT (tok_arg lexbuf (comment [])) }
  | raw newline  { new_line lexbuf; RAW (lexeme lexbuf) }
  | raw          { RAW (lexeme lexbuf) }
  | ['{' '}']    { RAW (lexeme lexbuf) }
  | eof          { EOF }

{
   (* Trim whitespace around standalone tags.

      The Mustache specification is careful with its treatment of
      whitespace. In particular, tags that do not themselves expand to
      visible content are defined as "standalone", with the
      requirement that if one or several standalone tags "stand alone"
      in a line (there is nothing else but whitespace), the whitespace
      of this line should be ommitted.

      For example, this means that:
        {{#foo}}
        I can access {{var}} inside the section.
        {{/foo}
      takes, once rendered, only 1 line instead of 3: the newlines
      after {{#foo}} and {{/foo}} are part of the "standalone
      whitespace", so they are not included in the output.

      Note: if a line contains only whitespace, no standalone tag,
      then the whitespace is preserved.

      We implement this by a post-processing past on the lexer token
      stream.  We split the token stream, one sub-stream per line, and
      then for each token line we determine if satisfies the
      standalone criterion.

      Another information collected at the same time, as it is also
      part of whitespace processing, is the "indentation" of partials:
      if a partial expands to multi-line content, and if it is
      intended at the use-site (it is at a non-zero column with only
      whitespace before it on the line), then the specification
      mandates that all its lines should be indented by the same
      amount.  We collect this information during the whitespace
      postprocessing of tokens, and store it in the Partial
      constructor as the first parameter.
   *)
   let handle_standalone lexer lexbuf =
     let ends_with_newline s =
       String.length s > 0 &&
       s.[String.length s - 1] = '\n'
     in
     let get_loc () = lexbuf.Lexing.lex_curr_p in
     let get_tok () =
       let loc_start = get_loc () in
       let tok = lexer lexbuf in
       let loc_end = get_loc () in
       (tok, loc_start, loc_end)
     in
     let slurp_line () =
       let rec loop acc =
         let tok = get_tok () in
         match tok with
         | EOF, _, _ -> tok :: acc
         | RAW s, _, _ when ends_with_newline s -> tok :: acc
         | _ -> loop (tok :: acc)
       in
       List.rev (loop [])
     in
     let is_blank s =
       let ret = ref true in
       for i = 0 to String.length s - 1 do
         if not (List.mem s.[i] [' '; '\t'; '\r'; '\n']) then
           ret := false
       done;
       !ret
     in
     let skip_blanks l =
       let rec loop skipped = function
         | (RAW s, _, _) :: toks when is_blank s ->
           loop (skipped + String.length s) toks
         | toks -> (skipped, toks)
       in
       loop 0 l
     in
     let trim_standalone toks =
       let toks =
         (* if the line starts with a partial,
            turn the skipped blank into partial indentation *)
         let (skipped, toks_after_blank) = skip_blanks toks in
         match toks_after_blank with
         | (PARTIAL (_      , name), loc1, loc2) :: rest ->
           (PARTIAL (skipped, name), loc1, loc2) :: rest
         | _ -> toks
       in
       let toks =
         (* if the line only contains whitespace and at least one standalone tags,
            remove all whitespace *)
         let rec standalone acc = function
           | (RAW s, _, _) :: rest when is_blank s ->
             (* omit whitespace *)
             standalone acc rest
           | ((OPEN_SECTION _
              | OPEN_INVERTED_SECTION _
              | CLOSE_SECTION _
              | PARTIAL _
              | COMMENT _), _, _) as tok :: rest ->
             (* collect standalone tags *)
             standalone (tok :: acc) rest
           | [] | (EOF, _, _) :: _ ->
             (* end of line *)
             if (acc = []) then
               (* if acc is empty, the line only contains whitespace,
                  which should be kept *)
               None
             else
               Some (List.rev acc)
           | _non_blank :: _rest ->
             (* non-blank, non-standalone token *)
             None
         in
         match standalone [] toks with
         | None -> toks
         | Some standalone_toks -> standalone_toks
       in
       assert (toks <> []);
       toks
     in
     let buffer = ref [] in
     fun () ->
       let toks = match !buffer with
         | (_ :: _) as toks -> toks
         | [] -> trim_standalone (slurp_line ())
       in
       buffer := List.tl toks; List.hd toks
}
