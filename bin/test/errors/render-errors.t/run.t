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
  Template render error:
  File "missing-variable.mustache", line 14, characters 40-46:
  the variable 'na' is missing (inside section 'name').
  [2]

  $ mustache missing-variable.json reference.mustache
  Template render error:
  File "reference.mustache", line 5, characters 4-12:
  the variable 'data' is missing (inside section 'list').
  [2]

Invalid section name:

  $ mustache reference.json missing-section.mustache
  Template render error:
  File "missing-section.mustache", line 14, characters 0-55:
  the section 'na' is missing.
  [2]

  $ mustache missing-section.json reference.mustache
  Template render error:
  File "reference.mustache", lines 9-12, characters 0-10:
  the section 'group' is missing.
  [2]

Error in a dotted path foo.bar (one case for the first component, the other in the second).

  $ mustache reference.json invalid-dotted-name-1.mustache
  Template render error:
  File "invalid-dotted-name-1.mustache", line 10, characters 2-15:
  the variable 'gro' is missing (inside section 'group').
  [2]

  $ mustache invalid-dotted-name-1.json reference.mustache
  Template render error:
  File "reference.mustache", lines 9-12, characters 0-10:
  the section 'group' is missing.
  [2]

  $ mustache reference.json invalid-dotted-name-2.mustache
  Template render error:
  File "invalid-dotted-name-2.mustache", line 10, characters 2-15:
  the variable 'group.fir' is missing (inside section 'group').
  [2]

  $ mustache invalid-dotted-name-2.json reference.mustache
  Template render error:
  File "reference.mustache", line 10, characters 2-17:
  the variable 'group.first' is missing (inside section 'group').
  [2]

Non-scalar used as a scalar:

  $ mustache reference.json non-scalar.mustache
  Template render error:
  File "non-scalar.mustache", line 4, characters 0-8:
  the value of 'list' is not a valid scalar.
  [2]

  $ mustache non-scalar.json reference.mustache
  Template render error:
  File "reference.mustache", line 1, characters 7-16:
  the value of 'title' is not a valid scalar.
  [2]

Missing partial (currently the CLI does not support any partial anyway):
(this file has a z- prefix so that the files do come in pairs
(this one does not) are all before in the alphabetic order, resulting
in better `ls` output).

  $ mustache reference.json z-missing-partial.mustache
  Template render error:
  File "z-missing-partial.mustache", line 11, characters 2-13:
  the partial 'second' is missing (inside section 'group').
  [2]
