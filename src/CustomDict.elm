module CustomDict exposing (Dict, all, fromList, get, insert, map, toList)


type Dict k v
    = D (List ( k, v ))


fromList : List ( k, v ) -> Dict k v
fromList items =
    D items


get : k -> Dict k v -> Maybe v
get key (D dict) =
    case dict of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if key == k then
                Just v

            else
                get key (D rest)


insert : k -> v -> Dict k v -> Dict k v
insert key value (D dict) =
    let
        removeKey ( k, v ) =
            k /= key
    in
    D (dict |> List.filter removeKey |> (::) ( key, value ))


map : (k -> v -> b) -> Dict k v -> Dict k b
map f (D dict) =
    D (List.map (\( k, v ) -> ( k, f k v )) dict)


all : (v -> Bool) -> Dict k v -> Bool
all pred (D dict) =
    List.all pred (List.map Tuple.second dict)


toList : Dict k v -> List ( k, v )
toList (D dict) =
    dict
