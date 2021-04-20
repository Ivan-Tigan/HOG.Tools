module HOG.Tools.TreeZipper

open FSharpx.Collections.Experimental
open FSharpPlus

type TreeZipper<'a> = {
    left_siblings: EagerRoseTree<'a> list
    right_siblings: EagerRoseTree<'a> list
    focus: 'a
    children: EagerRoseTree<'a> list
    parents: (EagerRoseTree<'a> list * 'a * EagerRoseTree<'a> list) list
}
let mv_left tz = {
    tz with
        left_siblings = tz.left_siblings.Tail
        right_siblings = EagerRoseTree.create tz.focus tz.children :: tz.right_siblings
        focus = tz.left_siblings.Head.Root
        children = tz.left_siblings.Head.Children
        parents = tz.parents
}
let mv_right tz = {
    tz with
        right_siblings = tz.right_siblings.Tail
        left_siblings = EagerRoseTree.create tz.focus tz.children :: tz.left_siblings
        focus = tz.right_siblings.Head.Root
        children = tz.right_siblings.Head.Children
        parents = tz.parents
}
let mv_down tz = {
    tz with
        right_siblings = tz.children.Tail
        left_siblings = []
        focus = tz.children.Head.Root
        children = tz.children.Head.Children
        parents = (tz.left_siblings, tz.focus, tz.right_siblings) :: tz.parents
}
let mv_up tz = {
    tz with
        right_siblings = match head tz.parents with _,_,x -> x
        left_siblings = match head tz.parents with x,_,_ -> x
        focus = match tz.parents with [_,x,_] -> x
        children = (rev tz.left_siblings) ++ [EagerRoseTree.create tz.focus tz.children] ++ tz.right_siblings
        parents = List.tail tz.parents
}
let modify f tz = { tz with focus = f tz.focus }
let mk_tree_zipper ert = {
    focus = ert.Root
    left_siblings = []
    right_siblings = []
    parents = []
    children = ert.Children
}
let tree = EagerRoseTree.create
let t1 =
    tree ("root", 0) [
        tree ("menu1/hbox", 1) [
            tree ("submenu1", 2) [
                tree ("play_normal", 3) []
                tree ("play_tournament", 3) []
            ]
            tree ("submenu2", 2) []
        ]
        tree ("menu2", 1) [
            tree ("sunmenu1", 2) []
            tree ("submenu2", 2) []
        ]
    ]
let t1z = mk_tree_zipper t1
let t1z2 = t1z |> mv_down |> mv_down |> mv_right |> modify (fun (s,i) -> s, i+1)
