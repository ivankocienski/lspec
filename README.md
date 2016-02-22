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



## API Documentation

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

Macro that wraps a spec-block. Inside the block a few forms are defined in
the context of the sexp; around-each, context and it.

*around-each* is a way of executing code around each spec. Around functions
are chained (parent functions are all gathered up and run in sequence before
a spec). You MUST use a yield in each around-each or the spec chain won't
continue. This may be a feature.

*context* is a way of grouping specs. Can be useful if you want to have
lots of specs for one thing (e.g. one function call or http path). Contexts
can be empty. Requires a description.

*it* is the way an individual spec is defined. If no body is provided it will
be 'pending'. This allows you to sketch out a skeleton of pending specs and
then code them one by one. Inside an it form you can use a pending decleration
to skip this spec. Useful if you want to keep the spec code to fix later but
need to deal with a lower level spec first. It blocks then use expect forms
to actually define tests.

See examples/ for a typical usage.

    (expect VAR TEST ...)

Macro that defines an expecation. VAR is the variable to test and TEST is
the particular test to run. Optional arguments may be provided depending on
the test. If something fails a spec a message declaring the ID of the spec,
the variable, value of variable and test will be printed. Useful, no?

    (list-expectations)

List all known expectations that lspec can match against with a short
description.

    (defexpectation TEST DESCRIPTION BODY)

Define custom expectations. At some point your app will have some fiddly
custom doodad that needs verifying (e.g. CSS) and you can define your own
expectations (or redefine existing expecations if you are evil).

TEST identifies the test for use in expect. DESCRIPTION is a short string
that gets printed when list-expectations or an expect block fails its test.

BODY is the body of the test. Return value of T indicates pass. Anything
else is a failure. 'val' is the value of the variable under test.

See src/expectations.lisp for some samples

    (current-formatter)

Show current formatter.

    (set-formatter)

Set formatter.

    (list-formatters)

List formatters that lspec has available.