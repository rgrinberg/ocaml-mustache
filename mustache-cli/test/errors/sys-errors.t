  $ echo "{}" > foo.json
  $ echo "" > foo.mustache

Nonexistent json file:
  $ mustache nonexistent.json foo.mustache
  mustache: DATA.json argument: no `nonexistent.json' file or directory
  Usage: mustache [OPTION]... DATA.json TEMPLATE.mustache
  Try `mustache --help' for more information.
  [124]

Nonexistent template file:
  $ mustache foo.json nonexistent.mustache
  mustache: TEMPLATE.mustache argument: no `nonexistent.mustache' file or
            directory
  Usage: mustache [OPTION]... DATA.json TEMPLATE.mustache
  Try `mustache --help' for more information.
  [124]
