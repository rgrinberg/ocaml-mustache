  $ echo "{}" > foo.json
  $ echo "" > foo.mustache

Nonexistent json file:
  $ mustache nonexistent.json foo.mustache
  Fatal error: exception Sys_error("nonexistent.json: No such file or directory")
  [2]

Nonexistent template file:
  $ mustache foo.json nonexistent.mustache
  Fatal error: exception Sys_error("nonexistent.mustache: No such file or directory")
  [2]
