Mandrill API in Haskell
===========================

Tests
-----

Configure the project:

```sh
$ cabal configure --enable-tests
```

To run tests, either execute

```sh
$ cabal test --show-details=always --test-option=--color
```

or start the repl with the `spec` target like so:

```sh
$ cabal repl spec
```

Then, execute the test suite:

```sh
λ :main
..hack..hack.hack
λ :reload
λ :main
```

Or, if you like [`guard-haskell`][0], just install and run guard:

```sh
$ bundle
$ guard
```

Caveat: this will only work correctly if you have configured the project with
`--enable-test` as described above.




  [0]: https://github.com/supki/guard-haskell
