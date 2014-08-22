%{
type t =
  | Iter_var
  | String of string
  | Escaped of string
  | Section of section
  | Unescaped of string
  | Partial of string
  | Concat of t list
%}
%token ITER_START
%token END
%token ESCAPE_START
%token ESCAPE_END
%token SECTION_START
%token SECTION_END
%token PARTIAL_START
%token UNESCAPE_START
%token <char> CHAR
%start mustache
%type <t> mustache

%%

mustache:
  | ITER_START

%%
