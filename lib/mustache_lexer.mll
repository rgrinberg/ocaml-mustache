{
exception Eof
}

rule mustache = parse
| "{{"  -> { ITER_START }
| "}}"  -> { END }
| "{{{" -> { ESCAPE_START }
| "}}}" -> { ESCAPE_END }
| "{{#" -> { SECTION_START }
| "{{/"  -> { SECTION_END }
| "{{>" -> { PARTIAL_START }
| "{{"  -> { UNESCAPE_START }
| _  -> { CHAR(x) }
| eof -> { raise Eof }
