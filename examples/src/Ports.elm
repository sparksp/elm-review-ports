port module Ports exposing
    ( action
    , load, play, stop
    )

{-| All ports are stored in this module to avoid naming collisions.


## Inbound Port

@docs action


## Alarm

@docs load, play, stop

-}


{-| This port is exposed and could be subscribed to.
-}
port action : (String -> msg) -> Sub msg


{-| This is an opaque port, only used through the exposed functions.
-}
port alarm : String -> Cmd msg


{-| This port is intentionally not exported, it should never be used.
-}
port unused : String -> Cmd msg


{-| Load the alarm
-}
load : Cmd msg
load =
    alarm "load"


{-| Play the alarm
-}
play : Cmd msg
play =
    alarm "play"


{-| Stop the alarm
-}
stop : Cmd msg
stop =
    alarm "stop"


{-| This function is intentionally not exported, it should never be used.
-}
doUnused : Cmd msg
doUnused =
    unused ""
