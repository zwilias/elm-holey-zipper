module List.Holey.Zipper
    exposing
        ( Full
        , Hole
        , Zipper
        , after
        , afterLast
        , append
        , before
        , beforeFirst
        , current
        , empty
        , findBackward
        , findForward
        , first
        , insertAfter
        , insertBefore
        , last
        , map
        , mapAfter
        , mapBefore
        , mapCurrent
        , mapParts
        , next
        , nextHole
        , plug
        , prepend
        , previous
        , previousHole
        , remove
        , singleton
        , toList
        , zipper
        )

{-| Like a regular old list-zipper, except it can also focus on the holes
_between_ elements.

This means you can represent an empty list, or point between two items and plug
that hole with a value.


# Types

@docs Zipper, Full, Hole


# Creation

@docs empty, singleton, zipper


# Extraction

@docs current, before, after, toList


# Navigation

@docs next, previous, nextHole, previousHole, first, last, beforeFirst, afterLast, findForward, findBackward


# Modification

@docs map, mapCurrent, mapBefore, mapAfter, mapParts, plug, remove, append, prepend, insertAfter, insertBefore

-}


{-| Represents `Zipper` over a list with items of type `a`. The type `t` will,
in practice, always be either `Full` or `Hole`. When it is `Full`, the zipper is
focused on an item. When it is `Hole`, you're looking at a hole between elements.
-}
type Zipper t a
    = Zipper (List a) (Maybe a) (List a)


{-| A `Zipper Full a` is pointing at an element of type `a`.
-}
type Full
    = Full


{-| A `Zipper Hole a` is pointing at a hole between `a`s.

... Heh.

-}
type Hole
    = Hole


{-| Get the value the `Zipper` is currently pointing at.

Only applicable to zippers pointing at a value.

    import List.Holey.Zipper as Zipper


    Zipper.singleton "hi there"
        |> Zipper.current
    --> "hi there"


    Zipper.zipper 1 [ 2, 3, 4 ]
        |> Zipper.last
        |> Zipper.current
    --> 4

-}
current : Zipper Full a -> a
current (Zipper _ f _) =
    let
        {- Through the type-system, we ensure this never happens.
           It's a pinky promise.
        -}
        unsafe : Maybe a -> a
        unsafe m =
            case m of
                Just v ->
                    v

                Nothing ->
                    -- KABOOM
                    unsafe m
    in
    unsafe f


{-| Create an empty zipper. It's pointing at nothing, there's nothing before it
and nothing after it. It's the loneliest of all zippers.

    import List.Holey.Zipper as Zipper


    Zipper.toList Zipper.empty
    --> []

-}
empty : Zipper Hole a
empty =
    Zipper [] Nothing []


{-| A zipper with a single thing in it. Singleton is just fancy-speak for single
thing.

    import List.Holey.Zipper as Zipper


    Zipper.singleton "wat"
        |> Zipper.toList
    --> [ "wat" ]

-}
singleton : a -> Zipper Full a
singleton v =
    Zipper [] (Just v) []


{-| Construct a zipper from the head of a list and some elements to come after
it.

    import List.Holey.Zipper as Zipper


    Zipper.zipper "foo" []
    --> Zipper.singleton "foo"


    Zipper.zipper 0 [ 1, 2, 3 ]
        |> Zipper.toList
    --> [ 0, 1, 2, 3 ]

-}
zipper : a -> List a -> Zipper Full a
zipper v after =
    Zipper [] (Just v) after


{-| List the things that come before the current location in the zipper.

    import List.Holey.Zipper as Zipper


    Zipper.zipper 0 [ 1, 2, 3 ]
        |> Zipper.next
        |> Maybe.andThen Zipper.next
        |> Maybe.map Zipper.before
    --> Just [ 0, 1 ]

-}
before : Zipper t a -> List a
before (Zipper b _ _) =
    List.reverse b


{-| Conversely, list the things that come after the current location.

    import List.Holey.Zipper as Zipper


    Zipper.zipper 0 [ 1, 2, 3 ]
        |> Zipper.next
        |> Maybe.map Zipper.after
    --> Just [ 2, 3 ]

-}
after : Zipper t a -> List a
after (Zipper _ _ a) =
    a


{-| Move the zipper to the next item, if there is one.

    import List.Holey.Zipper as Zipper


    Zipper.zipper 0 [ 1, 2, 3 ]
        |> Zipper.next
        |> Maybe.map Zipper.current
    --> Just 1

This also works from within holes:

    Zipper.empty
        |> Zipper.insertAfter "foo"
        |> Zipper.next
    --> Just <| Zipper.singleton "foo"

If there is no `next` thing, `next` is `Nothing`.

    Zipper.empty
        |> Zipper.next
    --> Nothing


    Zipper.zipper 0 [ 1, 2, 3 ]
        |> Zipper.last
        |> Zipper.next
    --> Nothing

-}
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


{-| Move the zipper to the previous item, if there is one.

    import List.Holey.Zipper as Zipper


    Zipper.previous Zipper.empty
    --> Nothing


    Zipper.zipper "hello" [ "holey", "world" ]
        |> Zipper.last
        |> Zipper.previous
        |> Maybe.map Zipper.current
    --> Just "holey"

-}
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


{-| Move the zipper to the hole right after the current item. A hole is a whole
lot of nothingness, so it's always there.

    import List.Holey.Zipper as Zipper


    Zipper.zipper "hello" [ "world" ]
        |> Zipper.nextHole
        |> Zipper.plug "holey"
        |> Zipper.toList
    --> [ "hello", "holey", "world" ]

-}
nextHole : Zipper Full a -> Zipper Hole a
nextHole ((Zipper b _ a) as z) =
    Zipper (current z :: b) Nothing a


{-| Move the zipper to the hole right before the current item. Feel free to plug
that hole right up!

    import List.Holey.Zipper as Zipper


    Zipper.singleton "world"
        |> Zipper.previousHole
        |> Zipper.plug "hello"
        |> Zipper.toList
    --> [ "hello", "world" ]

-}
previousHole : Zipper Full a -> Zipper Hole a
previousHole ((Zipper b _ a) as z) =
    Zipper b Nothing (current z :: a)


{-| Plug a hole-y zipper.

    import List.Holey.Zipper as Zipper


    Zipper.plug "plug" Zipper.empty
    --> Zipper.singleton "plug"

-}
plug : a -> Zipper Hole a -> Zipper Full a
plug v (Zipper b _ a) =
    Zipper b (Just v) a


{-| Punch a hole into the zipper by removing an element entirely. You can think
of this as collapsing the holes around the element into a single hole, but
honestly the holes are everywhere.

    import List.Holey.Zipper as Zipper


    Zipper.zipper "hello" [ "holey", "world" ]
        |> Zipper.next
        |> Maybe.map Zipper.remove
        |> Maybe.map Zipper.toList
    --> Just [ "hello", "world" ]

-}
remove : Zipper Full a -> Zipper Hole a
remove (Zipper b _ a) =
    Zipper b Nothing a


{-| Insert something after the current location.

    import List.Holey.Zipper as Zipper


    Zipper.empty
        |> Zipper.insertAfter "hello"
        |> Zipper.toList
    --> [ "hello" ]


    Zipper.zipper 123 [ 789 ]
        |> Zipper.insertAfter 456
        |> Zipper.toList
    --> [ 123, 456, 789 ]

-}
insertAfter : a -> Zipper t a -> Zipper t a
insertAfter v (Zipper b c a) =
    Zipper b c (v :: a)


{-| Insert something before the current location.

    import List.Holey.Zipper as Zipper


    Zipper.empty
        |> Zipper.insertBefore "hello"
        |> Zipper.toList
    --> [ "hello" ]


    Zipper.singleton 123
        |> Zipper.insertBefore 456
        |> Zipper.toList
    --> [ 456, 123 ]

-}
insertBefore : a -> Zipper t a -> Zipper t a
insertBefore v (Zipper b c a) =
    Zipper (v :: b) c a


{-| Flattens the zipper (back) into a list.

    import List.Holey.Zipper as Zipper


    Zipper.toList Zipper.empty
    --> []


    Zipper.zipper 123 [ 789 ]
        |> Zipper.nextHole
        |> Zipper.plug 456
        |> Zipper.toList
    --> [ 123, 456, 789 ]

-}
toList : Zipper t a -> List a
toList (Zipper b c a) =
    case c of
        Nothing ->
            List.reverse b ++ a

        Just v ->
            List.reverse b ++ v :: a


{-| Append a bunch of items after the zipper. This appends all the way at the end.

    import List.Holey.Zipper as Zipper


    Zipper.zipper 123 [ 456 ]
        |> Zipper.append [ 789, 0 ]
        |> Zipper.toList
    --> [ 123, 456, 789, 0 ]

-}
append : List a -> Zipper t a -> Zipper t a
append xs (Zipper b c a) =
    Zipper b c (a ++ xs)


{-| Prepend a bunch of things to the zipper. All the way before anything else.

    import List.Holey.Zipper as Zipper


    Zipper.zipper 1 [ 2, 3, 4 ]
        |> Zipper.last
        |> Zipper.prepend [ 5, 6, 7 ]
        |> Zipper.toList
    --> [ 5, 6, 7, 1, 2, 3, 4 ]

-}
prepend : List a -> Zipper t a -> Zipper t a
prepend xs (Zipper b c a) =
    Zipper (b ++ List.reverse xs) c a


{-| Go to the first element in the Zipper. Note that this only accepts a zipper
that points at a thing, since it would have to return a `Maybe` otherwise.

If you want to zip back to the beginning of a zipper pointing at a hole, you can
still use `zipper |> previous |> Maybe.map first`

    import List.Holey.Zipper as Zipper


    Zipper.zipper 1 [ 2, 3, 4 ]
        |> Zipper.prepend [ 4, 3, 2 ]
        |> Zipper.first
        |> Zipper.current
    --> 4

-}
first : Zipper Full a -> Zipper Full a
first ((Zipper b c a) as zipper) =
    case List.reverse b of
        [] ->
            zipper

        x :: xs ->
            Zipper [] (Just x) (xs ++ current zipper :: a)


{-| Go to the last element of a zipper. Same warnings as for `first` apply.

    import List.Holey.Zipper as Zipper


    Zipper.zipper 1 [ 2, 3, 4 ]
        |> Zipper.last
        |> Zipper.current
    --> 4

-}
last : Zipper Full a -> Zipper Full a
last ((Zipper b c a) as zipper) =
    case List.reverse a of
        [] ->
            zipper

        x :: xs ->
            Zipper (xs ++ current zipper :: b) (Just x) []


{-| Go to the hole before the first element. Remember that holes surround
everything! They are everywhere.

    import List.Holey.Zipper as Zipper


    Zipper.zipper 1 [ 3, 4 ]
        -- now we're after 1
        |> Zipper.nextHole
        -- plug that hole
        |> Zipper.plug 2
        -- back to _before_ the first element
        |> Zipper.beforeFirst
        -- put something in that hole
        |> Zipper.plug 0
        -- and check the result
        |> Zipper.toList
    --> [ 0, 1, 2, 3, 4 ]

-}
beforeFirst : Zipper t a -> Zipper Hole a
beforeFirst ((Zipper b c a) as zipper) =
    case c of
        Nothing ->
            Zipper [] Nothing (List.reverse b ++ a)

        Just v ->
            Zipper [] Nothing (List.reverse b ++ v :: a)


{-| Go to the hole after the end of the zipper. Into the nothingness.
-}
afterLast : Zipper t a -> Zipper Hole a
afterLast ((Zipper b c a) as zipper) =
    case c of
        Nothing ->
            Zipper (List.reverse a ++ b) Nothing []

        Just v ->
            Zipper (List.reverse a ++ v :: b) Nothing []


{-| Find the first element in the zipper the matches a predicate, returning a
zipper pointing at that thing if it was found. When provided with a zipper
pointing at a thing, that thing is also checked.

This start from the current location, and searches towards the end.

-}
findForward : (a -> Bool) -> Zipper t a -> Maybe (Zipper Full a)
findForward predicate ((Zipper b c a) as zipper) =
    if Maybe.withDefault False (Maybe.map predicate c) then
        Just <| Zipper b c a
    else
        next zipper |> Maybe.andThen (findForward predicate)


{-| Find the first element in the zipper matching a predicate, moving backwards
from the current position.
-}
findBackward : (a -> Bool) -> Zipper t a -> Maybe (Zipper Full a)
findBackward predicate ((Zipper b c a) as zipper) =
    if Maybe.withDefault False (Maybe.map predicate c) then
        Just <| Zipper b c a
    else
        previous zipper |> Maybe.andThen (findBackward predicate)


{-| Execute a function on every item in the zipper.

    import List.Holey.Zipper as Zipper


    Zipper.zipper "first" [ "second", "third" ]
        |> Zipper.map String.toUpper
        |> Zipper.toList
    --> [ "FIRST", "SECOND", "THIRD" ]

-}
map : (a -> b) -> Zipper t a -> Zipper t b
map f (Zipper b c a) =
    Zipper (List.map f b) (Maybe.map f c) (List.map f a)


{-| Execute a function on the current item in the zipper, when pointing at an
item.

    import List.Holey.Zipper as Zipper


    Zipper.zipper "first" [ "second", "third" ]
        |> Zipper.mapCurrent String.toUpper
        |> Zipper.toList
    --> [ "FIRST", "second", "third" ]

-}
mapCurrent : (a -> a) -> Zipper t a -> Zipper t a
mapCurrent f (Zipper b c a) =
    Zipper b (Maybe.map f c) a


{-| Execute a function on all the things that came before the current location.

    import List.Holey.Zipper as Zipper


    Zipper.zipper "first" [ "second" ]
        |> Zipper.nextHole
        |> Zipper.mapBefore String.toUpper
        |> Zipper.toList
    --> [ "FIRST", "second" ]

-}
mapBefore : (a -> a) -> Zipper t a -> Zipper t a
mapBefore f (Zipper b c a) =
    Zipper (List.map f b) c a


{-| Execute a function on all the things that come after the current location.
-}
mapAfter : (a -> a) -> Zipper t a -> Zipper t a
mapAfter f (Zipper b c a) =
    Zipper b c (List.map f a)


{-| Execute a triplet of functions on the different parts of a zipper - what
came before, what comes after, and the current thing if there is one.

    import List.Holey.Zipper as Zipper


    Zipper.zipper "first" [ "second" ]
        |> Zipper.nextHole
        |> Zipper.plug "one-and-a-halfth"
        |> Zipper.mapParts
            { before = (++) "before: "
            , current = (++) "current: "
            , after = (++) "after: "
            }
        |> Zipper.toList
    --> [ "before: first"
    --> , "current: one-and-a-halfth"
    --> , "after: second"
    --> ]

-}
mapParts :
    { before : a -> b
    , current : a -> b
    , after : a -> b
    }
    -> Zipper t a
    -> Zipper t b
mapParts conf (Zipper b c a) =
    Zipper (List.map conf.before b) (Maybe.map conf.current c) (List.map conf.after a)
