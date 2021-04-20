module HOG.Tools.Lerp

open FixMath.NET
open HOG.Tools.Extras
open HOG.Tools.Physics
open FSharpPlus
open FSharpx.Collections
open HOG.Tools.Timer
let lerp = Ease.lerp
let loption lerp dt = Option.map2 (lerp dt)
let llist lerp dt = List.map2 (lerp dt)
let lseq lerp dt = Seq.map2 (lerp dt)
let lfix64 = Fix64.lerp
let lmap lerp (old: Map<'guid, 'a>) (next:Map<'guid, 'a>) =
     old
        |> Map.map (fun k v -> next.TryFind k |> Option.map (lerp v))
        |> Map.catOptionValues
        |> fun old -> Map.unionWith konst old next
let ltimer = Timer.lerp
