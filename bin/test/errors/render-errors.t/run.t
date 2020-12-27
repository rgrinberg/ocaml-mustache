reference.json and reference.mustache work well together, there is no error.
  $ mustache reference.json reference.mustache
  Title: Some Title
  
  List:
    - foo
    - bar
    - baz
  
  Group:
    First
    Second
  
  The variable "name" has value "Some Name".
  
  
  Last line.


Error cases. In many cases, there are two ways to get a given
rendering error, one is by making a mistake in the template
(compared to the reference version), the other is to make a mistake in
the JSON file. We exercise both ways in the tests, to see if the
context given by error messages makes it easy for users to understand
one possible source of error, or both, or none.

Invalid variable name:

  $ mustache reference.json missing-variable.mustache
  Fatal error: exception Mustache_types.Missing_variable("na")
  [2]

  $ mustache missing-variable.json reference.mustache
  Fatal error: exception Mustache_types.Missing_variable("data")
  [2]

Invalid section name:

  $ mustache reference.json missing-section.mustache
  Fatal error: exception Mustache_types.Missing_section("na")
  [2]

  $ mustache missing-section.json reference.mustache
  Fatal error: exception Mustache_types.Missing_section("group")
  [2]

Error in a dotted path foo.bar (one case for the first component, the other in the second).

  $ mustache reference.json invalid-dotted-name-1.mustache
  Fatal error: exception Mustache_types.Missing_variable("gro")
  [2]

  $ mustache invalid-dotted-name-1.json reference.mustache
  Fatal error: exception Mustache_types.Missing_section("group")
  [2]

  $ mustache reference.json invalid-dotted-name-2.mustache
  Fatal error: exception Mustache_types.Missing_variable("fir")
  [2]

  $ mustache invalid-dotted-name-2.json reference.mustache
  Fatal error: exception Mustache_types.Missing_variable("first")
  [2]

Non-scalar used as a scalar:

  $ mustache reference.json non-scalar.mustache
  Fatal error: exception Mustache_types.Invalid_param("Lookup.scalar: not a scalar")
  [2]

  $ mustache non-scalar.json reference.mustache
  Fatal error: exception Mustache_types.Invalid_param("Lookup.scalar: not a scalar")
  [2]

Missing partial (currently the CLI does not support any partial anyway):
(this file has a z- prefix so that the files do come in pairs
(this one does not) are all before in the alphabetic order, resulting
in better `ls` output).

  $ mustache reference.json z-missing-partial.mustache
  Fatal error: exception Mustache_types.Missing_partial("second")
  [2]
