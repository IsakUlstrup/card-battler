module CustomDict exposing
    ( Dict
    , all
    , fromList
    , get
    , insert
    , map
    , toList
    , update
    )


type Dict k v
    = Dict (List ( k, v ))


fromList : List ( k, v ) -> Dict k v
fromList items =
    Dict items


toList : Dict k v -> List ( k, v )
toList (Dict dict) =
    dict


get : k -> Dict k v -> Maybe v
get key (Dict dict) =
    case dict of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if key == k then
                Just v

            else
                get key (Dict rest)


insert : k -> v -> Dict k v -> Dict k v
insert key value (Dict dict) =
    let
        removeKey ( k, v ) =
            k /= key
    in
    Dict (dict |> List.filter removeKey |> (::) ( key, value ))


map : (k -> v -> b) -> Dict k v -> Dict k b
map f (Dict dict) =
    Dict (List.map (\( k, v ) -> ( k, f k v )) dict)


all : (v -> Bool) -> Dict k v -> Bool
all pred (Dict dict) =
    List.all pred (List.map Tuple.second dict)


update : k -> (v -> v) -> Dict k v -> Dict k v
update key f (Dict dict) =
    let
        updateHelper ( itemKey, itemValue ) =
            if itemKey == key then
                ( itemKey, f itemValue )

            else
                ( itemKey, itemValue )
    in
    Dict (List.map updateHelper dict)
