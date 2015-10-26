How to contribute
=================

Contributions should be made as github pull requests. The CI will perform two actions:

1. run the unit tests
2. measure the code coverage of the unit tests

Please run both these actions yourself before submitting the request to save time.

Running the unit tests
----------------------

To run the tests, run:
```
make test
```
Note the tests don't require special privileges so run as a non-privileged user.

Checking code coverage
----------------------

Code coverage is measured using [bisect](http://bisect.x9c.fr). To inspect
current coverage, run `make coverage` and the results should be output
file-by-file followed by a summary:
```
$ make coverage
...[ snip ]...
Summary:
 - 'binding' points: 1013/1089 (93.02%)
 - 'sequence' points: 202/206 (98.06%)
 - 'for' points: 3/3 (100.00%)
 - 'if/then' points: 86/112 (76.79%)
 - 'try' points: 3/5 (60.00%)
 - 'while' points: 1/1 (100.00%)
 - 'match/function' points: 84/126 (66.67%)
 - 'class expression' points: none
 - 'class initializer' points: none
 - 'class method' points: none
 - 'class value' points: none
 - 'toplevel expression' points: 2/2 (100.00%)
 - 'lazy operator' points: 30/30 (100.00%)
 - total: 1424/1574 (90.47%)
```
