module HOG.Tools.Cooldown

open HOG.Tools.Timer

type Cooldown<'a> = Active of 'a | Ready | Cooldown of Timer
let tick dt = function Cooldown t when not (Timer.completed t) -> Cooldown (Timer.tick dt t) | Cooldown t -> Ready | cd -> cd
let lerp dt a b = match a, b with | Cooldown t, Cooldown t' -> Cooldown (Timer.lerp dt t t') | _ -> b 
