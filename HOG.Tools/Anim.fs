module HOG.Tools.Anim

open HOG.Tools.Extras
open HOG.Tools.Physics
open HOG.Tools.Timer

type 'a Anim = private Anim of Timer * (fix64 -> 'a) 
let mk_animf t f = Anim(Timer.mk_timerf fix64.Zero t, f)
let mk_anim t f = mk_animf (fix64 t) f
let value = function Anim(t, f) -> f <| Timer.completion t
let timer = function Anim(t, f) -> t
let progress = function Anim(t, _) -> Timer.completion t
let rec tick dt = function Anim(t, f) -> Anim(Timer.tick dt t, f)
let a1 = mk_anim 4 (function t when t <= fix64 1 -> Vec.lerp t Vec.one (Vec.vec 5 5), 'a'| t when t <= fix64 3 -> Vec.lerp t (Vec.vec 5 5) Vec.one, 'b')
let v1 = a1 |> tick fix64.One |> value
