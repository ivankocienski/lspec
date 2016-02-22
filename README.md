# lspec

A 'sort of' clone of Ruby rspec in Common Lisp.

## Install

Unfortunately lspec is not a part of quicklisp at this time and must be
installed by cloning the repo into your local-projects directory manually.

  git clone https://github.com/ivankocienski/lspec.git ~/quicklisp/local-projects/lspec

You may need to run (ql:register-local-projects) to refresh databse.

## Quick start guide

Write some specs (see examples directory) then run

  (run-all)

to run all your specs. You will notice that failures and pending specs are
reported at the end of each run like so

```
---------------------------------------------


pending:
  (3.4) tests with callback blocks: is pending with a nice message
    "FIXME"
  (3.3) tests with callback blocks: is also pending
    "This spec is pending"
  (3.2) tests with callback blocks: should be pending
    "[is pending]"

failed:
  (3.1.2) A group of tests: a way of nesting and segmenting tests: will fail
    "(NOT-ZERO=1) should be zero"

results: 9 specs, 1 failures, 3 pending

done. 0.0009765625s
```

To run an individual spec again use its ID like so

  (run-select "3.1.2")

This will also run an entire 'spec group' like so

  (run-select "3.1")

## Documentation

  (clear-specs)

Reset all registered specs.

  (list-specs)

List specs (in a tree like format with IDs).

  (count-specs)

Count registered specs.

  (run-all)

Run all registered specs.

  (run-select filter)

Run selected specs based on filter string.

  (specify ...)

Macro that wraps a spec-block. See examples/ for a typical usage.

  (expect)

Macro that defines an expecation.

  (list-expectation)

List all known expectations that lspec can match against

  (defexpectation)

Define custom expectations.

  (current-formatter)

Show current formatter.

  (set-formatter)

Set formatter.

  (list-formatters)

List formatters that lspec has available.