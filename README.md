# review-ports

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect errant elm ports.

## Provided rules

- [`NoDuplicatePorts`](https://package.elm-lang.org/packages/sparksp/elm-review-ports/latest/NoDuplicatePorts) - Ensure that port names are unique across your project.

## Example configuration

```elm
module ReviewConfig exposing (config)

import NoDuplicatePorts
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoDuplicatePorts.rule
    ]
```
