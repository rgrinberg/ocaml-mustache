Simple test:
  $ mustache data.json foo.mustache
  Inside the include is "Foo Bar !"

Include in child or parent directory:
  $ mkdir subdir
  $ echo "Test from {{src}}" > subdir/test.mustache
  $ echo "{{> subdir/test }}" > from_parent.mustache
  $ echo '{ "src": "parent" }' > from_parent.json
  $ mustache from_parent.json from_parent.mustache
  Test from parent

  $ mkdir subdir/child
  $ echo "{{> ../test }}" > subdir/child/from_child.mustache
  $ echo '{ "src": "child" }' > subdir/child/from_child.json
  $ (cd subdir/child; mustache from_child.json from_child.mustache)
  Test from child

When working with templates from outside the current directory,
we need to set the search path to locate their included partials.

This fails:
  $ (cd subdir; mustache ../data.json ../foo.mustache)
  Template render error:
  File "../foo.mustache", line 2, characters 23-31:
  the partial 'bar' is missing.
  [2]

This works with the "-I .." option:
  $ (cd subdir; mustache -I .. ../data.json ../foo.mustache)
  Inside the include is "Foo Bar !"

Note that the include directory is *not* used to locate the template
(or data) argument. This fails:
  $ (cd subdir; mustache -I .. ../data.json foo.mustache)
  mustache: TEMPLATE.mustache argument: no `foo.mustache' file or directory
  Usage: mustache [OPTION]... DATA.json TEMPLATE.mustache
  Try `mustache --help' for more information.
  [124]

Search path precedence order.
  $ mkdir precedence
  $ mkdir precedence/first
  $ mkdir precedence/last
  $ echo "First" > precedence/first/include.mustache
  $ echo "Last" > precedence/last/include.mustache
  $ echo "{{>include}}" > precedence/template.mustache
  $ echo "{}" > precedence/data.json

The include directory added first (left -I option) has precedence
over the include directories added after:
  $ (cd precedence; mustache -I first -I last data.json template.mustache)
  First

The working directory has precedence over the include directories:
  $ echo "Working" > precedence/include.mustache
  $ (cd precedence; mustache -I first -I last data.json template.mustache)
  Working

... unless --no-working-dir is used:
  $ (cd precedence; mustache --no-working-dir -I first -I last data.json template.mustache)
  First
