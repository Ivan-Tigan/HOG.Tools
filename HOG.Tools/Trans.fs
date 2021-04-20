module HOG.Tools.Trans
open FSharpPlus
open HOG.Tools.Extras
open HOG.Tools.Timer

let (>>|) a f = a >> uncurry f
type 'a Trans = private Trans of 'a * 'a * Timer
let transt t fa a = Trans(a, fa a, t)
let trans t fa a = transt (Timer.mk_timer 0 t) fa a
let transf t fa a = transt (Timer.mk_timerf fix64.Zero t) fa a
let state a = trans 0 id a
let tick dt tr = function Trans(a,b,t) when Timer.completed t -> tr b | Trans(a,b,t) -> Trans(a,b, Timer.tick dt t)

let (|Trans|) = function Trans(a,b,t) -> Trans(a,b,t)  
let value lerp = function Trans(a,b,t) -> lerp (Timer.completion t) a b
//        let tzt1 = state TreeZipper.t1z2
//        let tzt2 = tzt1 |> tick float'.Zero (trans 1 TreeZipper.mv_down)
//        let v = match tzt2 with State t -> t.focus | Trans(a,b,t) -> b.focus 