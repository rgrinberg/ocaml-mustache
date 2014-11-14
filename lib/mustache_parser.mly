%{
  open Mustache_types
%}

%token EOF
%token END
%token <string> ESCAPE_START
%token ESCAPE_END
%token <string> UNESCAPE_START_AMPERSAND
%token <string> SECTION_INVERT_START
%token <string> SECTION_START
%token <string> SECTION_END
%token <string> PARTIAL_START
%token <string> UNESCAPE_START
%token UNESCAPE_END

%token <string> RAW
%token DOT

%start mustache
%type <Mustache_types.t> mustache

%%

section:
  | SECTION_START END mustache SECTION_END END
    {
      let (start_s, end_s) = ($1, $4) in
      if start_s = end_s
      then { contents=$3; name=start_s }
      else
        let msg =
          Printf.sprintf "Mismatched section %s with %s" start_s end_s in
        raise (Invalid_template msg)
    }

mustache_element:
  | UNESCAPE_START UNESCAPE_END { Unescaped $1 }
  | UNESCAPE_START_AMPERSAND ESCAPE_END { Unescaped $1 }
  | ESCAPE_START END { if $1 = "." then Iter_var else Escaped $1 }
  | SECTION_INVERT_START END { Inverted_section $1 }
  | PARTIAL_START END { Partial $1 }
  | section { Section $1 }

string:
  | RAW { String $1 }

mustache_l:
  | mustache_element mustache_l { ($1 :: $2) }
  | string mustache_l { ($1 :: $2) }
  | mustache_element { [$1] }
  | string { [$1] }

mustache:
  | mustache_l {
    match $1 with
    | [x] -> x
    | x -> Concat x
  }
  | EOF { String "" }

%%
