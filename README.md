# review-ports

![elm package](https://img.shields.io/elm-package/v/sparksp/elm-review-ports)
![elm-review 2.0](https://img.shields.io/badge/elm--review-2.0-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-ports/workflows/Tests/badge.svg)

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
