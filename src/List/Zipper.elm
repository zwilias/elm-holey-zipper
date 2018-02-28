module List.Zipper
    exposing
        ( Full
        , Hole
        , Zipper
        )


type Zipper t a
    = Zipper (List a) (Maybe a) (List a)


type Full
    = Full


type Hole
    = Hole


current : Zipper Full a -> a
current (Zipper _ f _) =
    let
        {- Through the type-system, we ensure this never happens. -}
        unsafe : Maybe a -> a
        unsafe m =
            case m of
                Just v ->
                    v

                Nothing ->
                    unsafe m
    in
    unsafe f


empty : Zipper Hole a
empty =
    Zipper [] Nothing []


singleton : a -> Zipper Full a
singleton v =
    Zipper [] (Just v) []


nextSpace : Zipper Full a -> Zipper Hole a
nextSpace ((Zipper b _ a) as z) =
    Zipper (current z :: b) Nothing a


before : Zipper t a -> List a
before (Zipper b _ _) =
    b


after : Zipper t a -> List a
after (Zipper _ _ a) =
    a


previousSpace : Zipper Full a -> Zipper Hole a
previousSpace ((Zipper b _ a) as z) =
    Zipper b Nothing (current z :: a)


next : Zipper t a -> Maybe (Zipper Full a)
next (Zipper b c a) =
    case a of
        [] ->
            Nothing

        n :: rest ->
            case c of
                Nothing ->
                    Just <| Zipper b (Just n) rest

                Just v ->
                    Just <| Zipper (v :: b) (Just n) rest


previous : Zipper t a -> Maybe (Zipper Full a)
previous (Zipper b c a) =
    case b of
        [] ->
            Nothing

        p :: rest ->
            case c of
                Nothing ->
                    Just <| Zipper rest (Just p) a

                Just v ->
                    Just <| Zipper rest (Just p) (v :: a)


mapCurrent : (a -> a) -> Zipper t a -> Zipper t a
mapCurrent f (Zipper b c a) =
    Zipper b (Maybe.map f c) a


set : a -> Zipper Hole a -> Zipper Full a
set v (Zipper b _ a) =
    Zipper b (Just v) a


remove : Zipper Full a -> Zipper Hole a
remove (Zipper b _ a) =
    Zipper b Nothing a


insertAfter : a -> Zipper t a -> Zipper t a
insertAfter v (Zipper b c a) =
    Zipper b c (v :: a)


insertBefore : a -> Zipper t a -> Zipper t a
insertBefore v (Zipper b c a) =
    Zipper (v :: b) c a


toList : Zipper t a -> List a
toList (Zipper b c a) =
    case c of
        Nothing ->
            List.reverse b ++ a

        Just v ->
            List.reverse b ++ [ v ] ++ a


append : List a -> Zipper t a -> Zipper t a
append xs (Zipper b c a) =
    Zipper b c (a ++ xs)
