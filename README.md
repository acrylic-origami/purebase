# purebase
As complete of a base implementation with as little magic as possible

## Why?

Static analysis tools with an abstract interpreter want to follow the code execution into Prelude functions, but not past the point where magic hashes start to show up and make life for the interpreter miserable. In particular, numeric types and functions (especially the unboxed ones); IO and ST; GHC's foreign pointers; as well as Text, Word and encodings, are all hard for even the GHC API to handle.

This is an attempt to balance keeping as many of the implementations here as possible, and directing the rest to the built-in libraries (outside of the `C` namespace).

Some big notes about the limitations:

- `Monad` is a tricky one: we want access to all the monad instances, but `do` defaults to returning the built-in monad.
- `deriving (Functor, Monad, Traversable)` don't work because they're implemented and included throughout here, but `deriving` doesn't have rules for these "custom implementations".
- A lot of the functions are yet untested: they could run into problems similar to above from functions and values that come from the GHC part of things (e.g. things that are `Data.Traversable` but not `C.Data.Traversable`). If the type system were duck-typed we'd be okay but it's not of course. Unforuntely this may mean the user will have to awkwardly refactor to make sure the typechecker knows we're talking about the same constraint when this is run.
