namespace HOG.Tools

open System.Threading.Tasks
open FSharp.Control.Tasks
open FixMath.NET
open FSharpPlus
open FSharpx.Collections

module Extras =
    let randomStr (chars:string) = 
        let charsLen = chars.Length
        let random = System.Random()

        fun len -> 
            let randomChars = [|for i in 0..len -> chars.[random.Next(charsLen)]|]
            new System.String(randomChars)

    let id_bind f = Map.mapValues f >> Map.catOptionValues
    type fix64 = Fix64
    let fract a b = (fix64 a)/(fix64 b)
    type Fix64 with
        static member nondeterministically f = fract (int (10000.f * f)) 10000
        static member get_One = Fix64.One
        static member pow a b = Fix64.Pow(a, b)
    //        static member pow a b =
    //            let rec pow acc a b =
    //                if b = float'.Zero || a = float'.Zero
    //                then acc
    //                else
    //                    if b > float'.Zero
    //                    then pow (a * acc) a (b - float'.One)
    //                    else pow acc (float'.One/a) (-b)
    //            pow float'.One a b
        static member Zero = Fix64.Zero
        static member sign n = if n > Fix64.Zero then Fix64.One else (if n < Fix64.Zero then -Fix64.One else Fix64.Zero)
        static member lerp dt start final = start * (fix64.One - dt) + final * dt
        member this.rad_to_deg = this * (fix64 180 / fix64.Pi)
        member this.deg_to_rad = this / (fix64 180 / fix64.Pi)
    module fix64 =
        let inline rad_to_deg (v:fix64) = v.rad_to_deg
        let inline deg_to_rad (v:fix64) = v.deg_to_rad
    let inline plus a b = a + b
    let inline clamp min max a = match () with | _ when a < min -> min | _ when a > max -> max | _ -> a
    let inline between low high v = low <= v && v <= high
    let if' c a b v = if c v then a v else b v
    let one_over a = fix64.One / fix64 a 
    let inline sign v = if v > fix64.Zero  then fix64.One else -fix64.One
    let (|>|) a f = let _ = f a in a
    let until p f = let rec go x = match () with | _ when p x -> x | _ -> go (f x) in go 
    let mapsnd f (a, b) = (a, f b)
    let fst3 (a,_,_) = a
    let snd3 (_,b,_) = b
    let thrd3 (_,_,c) = c
    let memoize fn =
         let cache = System.Collections.Generic.Dictionary<_,_>()
         (fun x ->
           match cache.TryGetValue x with
           | true, v -> v
           | false, _ -> let v = fn (x)
                         cache.Add(x,v)
                         v)
    let inline wrap max n = (n % max + max)%max  
    let uncurry f (a,b) = f a b
    let ordinal i =
        sprintf "%d%s" i <|
            match i with
            | 11 -> "th"
            | 12 -> "th"
            | 13 -> "th"
            | i when i%10 = 0 -> "th"
            | i when i%10 = 1 -> "st"
            | i when i%10 = 2 -> "nd"
            | i when i%10 = 3 -> "rd"
            | i -> "th"
    module Task =
        open TaskBuilder
        let (|Ready|Pending|) (t:Task<'a>) = if t.IsCompleted then Ready t.Result else Pending
        let timeout (n:int) (t:Task<'a> list) (f:'a) = task { let! r = (t ++ [task {let! _ = Task.Delay n in return f}]) |> Task.WhenAny in return! r}
        let rec wait_until b = task { if b then () else let! _ = Task.Delay 25 in return! wait_until b }
    