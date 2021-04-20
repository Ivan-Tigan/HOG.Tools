module HOG.Tools.Ease

open HOG.Tools.Extras

let inline lerp one dt start final = start * (one - dt) + final * dt
let inline square a = a * a 
let inline plus a b = a + b
let two = fix64 2
let one = fix64.One
let half = fix64(1)/fix64(2)
let linear = id
let reverse t = one - t
let in_quad (t:fix64) = t * t
let in_expo t = if t = fix64.Zero then fix64.Zero else fix64.pow two (fix64 10 * t - fix64 10)
let out_quad t = t * (fix64(2) - t)
let in_out_quad t = if t<half then two*t*t else -one + (two*two-two*t)*t
let in_out_expo t =
    match t with
    | t when t = fix64.Zero -> fix64.Zero
    | t when t = one -> one
    | t when t < half -> (fix64.pow two (fix64 20 * t - (fix64 10))) /two
    | t -> (two - fix64.pow two (-fix64 20 * t + fix64 10)) / two
let inline in_cubic t = t * t * t 
let in_out_quint t =  if t<half then fix64 16 * fix64.pow t (fix64 5) else (one - fix64.pow (-two * t + two) (fix64 5))/two

