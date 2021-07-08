module HOG.Tools.Random

open System

let modulus = Int32.MaxValue
let multiplier = 48271
let increment = 0
type MRandom = private {mutable seed:int}
    with
    member r.nexti =
        let n = (multiplier * r.seed + increment) % modulus
        r.seed <- n
        n
    member r.rangei min max =
        let dist = max - min
        (abs r.nexti) % dist + min
    member r.rangef (min:fix64) (max:fix64) =
        let min, max = int (fix64 10000 * min), int (fix64 10000 * max)
        let r = r.rangei min max
        fract r 10000
let mk_mrandom seed = {seed = seed}