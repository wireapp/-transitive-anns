# transitive-anns

> There are no constraints on the human mind, no walls around the human spirit,
> no barriers to our progress except those we ourselves erect.

## Overview

This package is a small compiler plugin that invisibly passes around unsolved
constraints for you. The idea is that you'd like to use unsolved constraints to
document some feature of your codebase, perhaps exposing this to servant.

Unfortunately, in the real world, passing around dozens of unsolved constraints
just for documentation feels a lot like doing the compiler's job for it.
`transitive-anns` automates this process, throwing the task right back to the
compiler.

Let's see how it works.

```haskell
{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

import TransitiveAnns.Types

test3 :: AddAnnotation 'Remote "hello from" "test3" x => Int
test3 = 4

test2 :: AddAnnotation 'Remote "hello from" "test2" x => Int
test2 = test3
```

The plugin with automatically solve these `AddAnnotation` constraints, meaning
we do not need to add a `AddAnnotation 'Remote "hello from" "test3" x` constraint
to `test2`.

When we'd like to re-expose these constraints, we can do this:

```haskell
test :: ToHasAnnotations x => Int
test = exposeAnnotations test2
```

where `ToHasAnnotations x` will magically expand to the following constraints:


```haskell
( HasAnnotation 'Remote "hello from" "test2"
, HasAnnotation 'Remote "hello from" "test3"
)
```

Solving a `ToHasAnnotations` constraint automatically looks at the transitive
closure of every function called by the argument of `exposeAnnotations`, adding
a 'HasAnnotation' constraint for every 'AddAnnotation' solved.

Better yet, it all works across module boundaries!


## Nitty Gritty Details

Internally, this plugin consists of two parts: a CoreToDo plugin, and a
typechecker plugin. The typechecker plugin is responsible for solving the
constraints, and in particular, it stashes `AddAnnotation` constraints away as
actual `ANN` annotations in the compiled `ModIface`. This ensures the
constraints are cached in the compiled artifacts and thus work across
module/package boundaries.

The CoreToDo runs after the module has been compiled, looks up every function
with a generated `ANN`, and then also attaches it to any function which *calls*
an annotated function. This mechanism is responsible for transitively
propagating the annotations across the call tree.

