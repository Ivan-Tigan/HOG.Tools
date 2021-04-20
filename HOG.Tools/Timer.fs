module HOG.Tools.Timer
open FixMath.NET
open HOG.Tools.Extras
type Timer = Timer of fix64 * fix64 with
    static member mk_timer t t' = Timer(fix64 t, fix64 t') 
    static member mk_timerf t t' = Timer(t, t') 
    static member duration = function | Timer(d, _) -> d
    static member max_time = function | Timer(_, m) -> m
    member this.completion = match this with Timer(d, m) -> d/m
    static member lerp dt (Timer(c, t)) (Timer(cf, tf) as t') = Timer((if c<cf && not <| Timer.completed t' then fix64.lerp dt c cf else cf), fix64.lerp dt t tf)
    static member completed t = Timer.duration t >= Timer.max_time t
    static member is_at_zero t = Timer.duration t = fix64.Zero
    static member running t = Timer.duration t > fix64.Zero
    static member running_not_completed t = Timer.duration t > fix64.Zero && not (Timer.completed t)
    static member tick delta t = Timer(t |> Timer.duration |> (+) delta |> Extras.clamp fix64.Zero (Timer.max_time t), Timer.max_time t)
    static member tick_or_restart delta b t = if b then t |> Timer.tick delta else Timer(fix64.Zero, Timer.max_time t)
    
module Timer =
    let completion = function | Timer(d, m) when m = fix64.Zero -> fix64 1 | Timer(d, m) -> d/m
    let completion' ease = completion >> ease
