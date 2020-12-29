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
  
