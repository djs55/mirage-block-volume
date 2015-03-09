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

Code coverage is measured using [bisect](http://bisect.x9c.fr). To inspect current
coverage, run:
```
make coverage
```
and inspect the output in `/vagrant/report/index.html`.
