module Energy exposing (Energy(..), toString)


type Energy
    = Cyan
    | Magenta
    | Yellow


toString : Energy -> String
toString energy =
    case energy of
        Cyan ->
            "cyan"

        Magenta ->
            "magenta"

        Yellow ->
            "yellow"
