# Holey Zipper

Like a `List` zipper, but with more holes in it.

The basic idea is a zipper that can represent an empty list, can focus before 
and after every item, and doesn't make life hard.

```elm
import List.Holey.Zipper as Zipper

Zipper.empty           -- Zipper Hole a
    |> Zipper.plug 5   -- Zipper Full Int
    |> Zipper.append
        [ 1, 2, 3 ]    -- Zipper Full Int
    |> Zipper.nextHole -- Zipper Hole Int
    |> Zipper.toList   -- List Int
--> [ 5, 1, 2, 3 ]
```

So, there's that.

---

Made with love and released under BSD-3.
