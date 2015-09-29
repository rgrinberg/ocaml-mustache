type t =
  | Iter_var
  | String of string
  | Escaped of string
  | Section of section
  | Unescaped of string
  | Partial of string
  | Inverted_section of section
  | Concat of t list
  | Comment of string
and section = {
  name: string;
  contents: t;
}

exception Invalid_param of string
exception Invalid_template of string
exception Missing_variable of string
