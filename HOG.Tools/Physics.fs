module HOG.Tools.Physics
open HOG.Tools.Extras
open FixMath.NET
open FSharpPlus

[<Struct>]
type Vec = Vec of fix64 * fix64 with
    member this.x = match this with | Vec(x, _) -> x
    member this.y = match this with | Vec(_, y) -> y
    member this.length = sqrt (this.x*this.x + this.y*this.y)
    static member nondeterministically fx fy = Vec(fix64.nondeterministically fx, fix64.nondeterministically fy)
    static member right = Vec(fix64.One, fix64.Zero)
    static member left = Vec(-fix64.One, fix64.Zero)
    static member up = Vec(fix64.Zero, -fix64.One)
    static member down = Vec(fix64.Zero, fix64.One)
    static member zero = Vec(fix64.Zero, fix64.Zero)
    static member one = Vec(fix64.One, fix64.One)
    static member (+) (a:Vec, b:Vec) = Vec(a.x + b.x, a.y + b.y)
    static member (-) (a:Vec, b:Vec) = Vec(a.x - b.x, a.y - b.y)
    static member lerp dt (Vec(x1, y1)) (Vec(x2, y2)) = Vec(fix64.lerp dt x1 x2, fix64.lerp dt y1 y2) 
    member this.rotate_towards p = this.direction_to p * this.length
    member this.distance_to (b:Vec) : fix64 = (b-this).length
    member this.direction_to (b:Vec) : Vec = (b-this).normalized
    /// if the distance to b > 1 then returns normalized direction, otherwise returns unnormalized direction (does not normalize the direction if it has length less than one)
    member this.direction_to' (b:Vec) : Vec = (b-this).normalized'
    static member (*) (a:Vec, b:fix64) = Vec.scaled b a
    static member scaled (s:fix64) (Vec(x, y): Vec)= Vec(x * s, y * s)
    member a.normalized = let l = a.length in if l = fix64.Zero then Vec.zero else Vec(a.x/l, a.y/l)
    member a.normalized' = let l = a.length in if l = fix64.Zero then Vec.zero else (if l < fix64.One then a else Vec(a.x/l, a.y/l))
    static member Zero = Vec.zero
    static member dot (a:Vec) (b:Vec) = a.x * b.x + a.y * b.y
    member this.slide normal = this - normal * Vec.dot this normal
    member this.bounce normal = if Vec.abs_angle_deg this normal > fix64 90 then this - Vec.scaled (fix64(2) * Vec.dot this normal) normal else this
    member this.rotated degrees = Vec((fix64.FastCos degrees) * this.x - (fix64.FastSin degrees) * this.y, (fix64.FastSin degrees) * this.x + (fix64.FastCos degrees) * this.y)
    member this.angle_origin_rad = fix64.Atan2(this.y, this.x)
    static member angle_rad (v1:Vec) (v2:Vec) = let v1, v2 = v1.normalized, v2.normalized in fix64.Atan2 (v1.y-v2.y,v1.x-v2.x)
    static member angle_deg v1 v2 = Vec.angle_rad v1 v2 |> fix64.rad_to_deg 
    static member abs_angle_rad (v1:Vec) (v2:Vec) = fix64.Acos (Vec.dot v1.normalized v2.normalized)
    static member abs_angle_deg v1 v2 = Vec.abs_angle_rad v1 v2 |> fix64.rad_to_deg 
    static member iso_to_screen w h (v:Vec) = Vec((v.x-v.y) * w, (v.x + v.y) * h/(fix64 2))
module Vec =
    let vec a b = Vec(fix64 a, fix64 b)
    let length (v:Vec) = v.length 
    let inline normalized (v:Vec) = v.normalized
[<Struct>]
type Shape = Circle of fix64 | Rect of fix64 * fix64
//    [<Struct>]
type Body = { position: Vec; velocity: Vec; area: Shape } with
    member b.direction = b.velocity.normalized'
    member b.speed = b.velocity.length
    static member basic x y shape = { position = Vec(fix64 x, fix64 y); velocity = Vec.zero; area = shape } 
    static member lerp dt (b:Body) towards = { b with position = Vec.lerp dt b.position towards.position; velocity = Vec.lerp dt b.velocity towards.velocity}
    static member translated (delta:fix64) b = {b with position = b.position + (b.velocity |> Vec.scaled delta)}
    static member surface_normal_towards b b' =
        match b.area with
        | Circle _ -> (b'.position - b.position) |> Vec.normalized
        | Rect(w, h) ->
            let rel_dir = Vec((b'.position.x - b.position.x) * h, (b'.position.y - b.position.y) * w)
            let ret = 
                match () with
                | _ when abs(rel_dir.x) >= abs(rel_dir.y) -> Vec.right * sign rel_dir.x
                | _ when abs(rel_dir.x) < abs(rel_dir.y) ->  Vec.down * sign rel_dir.y
                | _ -> failwith "That is not how you find surface normal for rectangle"
//                GD.Print("Normal from a to b is ", b, b', ret, " and rel dir ", rel_dir)
            ret
    static member overlapping b b' =
        match (b.area, b'.area) with
        | Circle(r), Circle(r') -> b.position.distance_to b'.position < r + r'
        | Rect(w, h), Rect(w', h') ->
            b.position.x - w < b'.position.x + w' &&
            b.position.x + w > b'.position.x  - w' &&
            b.position.y - h < b'.position.y + h' &&
            b.position.y + h > b'.position.y - h'
        | Rect(w, h), Circle(r) ->
            b.position.x - w < b'.position.x + r &&
            b.position.x + w > b'.position.x - r &&
            b.position.y - h < b'.position.y + r &&
            b.position.y + h > b'.position.y - r
        | Circle(_), Rect(_) -> Body.overlapping b' b
    static member just_overlapping dt (b:Body) (b':Body) =
        (b'.speed <> fix64.Zero || b.speed <> fix64.Zero) &&
            let translator = Body.translated dt in
            let not_overlapping_now = Body.overlapping b b' |> not
//            let i = int (dt * ((b.speed / fix64 1) + (b'.speed / fix64 1)))
//            let i = clamp 1 i i
            let i = 1
//            let will_overlap = [1..i] |> fold (fun acc j -> acc || Body.overlapping (translator {b with velocity = b.velocity * fract j i}) (translator {b' with velocity = b'.velocity * fract j i}) ) false
            let will_overlap = Body.overlapping (translator b) (translator b') 
            not_overlapping_now && will_overlap
    static member just_end_overlapping dt b b' = let translator = Body.translated dt in (Body.overlapping b b' &&  Body.overlapping (translator b) (translator b') |> not) //|>| (fun _ -> GD.Print "Just ended overlap")
    static member just_collision_normal dt b b' = if (Body.just_overlapping dt b b') then (Body.surface_normal_towards b b') else Vec.zero
    static member just_collisions_normal dt bs b' = (bs |> List.map (fun x -> Body.just_collision_normal dt x b') |> List.sum) |> Vec.normalized
    static member all_just_collisions_normals dt bs bs' = [for b' in bs' -> b', (bs |> List.map (fun x -> x, Body.just_collision_normal dt x b'))]
    static member move_and_collide dt bs b' = if bs |> List.exists (fun x -> Body.just_overlapping dt x b') then b' else Body.translated dt b'
    static member move_and_slide dt bs b' = Body.translated dt {b' with velocity = b'.velocity.slide <| Body.just_collisions_normal dt bs b'}
    static member move_and_slide_around dt bs b' = {Body.move_and_slide dt bs b' with velocity = b'.velocity}
    static member move_and_bounce dt bs b' = Body.translated dt {b' with velocity = b'.velocity.bounce <| Body.just_collisions_normal dt bs b'} //|>| (fun x -> GD.Print x.direction)
    static member collision_normal b = if' (Body.overlapping b) (Body.surface_normal_towards b) (konst Vec.zero)
    static member collisions_normal bs b' = (bs |> List.map (fun x -> Body.collision_normal x b') |> List.sum) |> Vec.normalized
    static member floor dt bs b' = bs |> List.filter (fun b -> let just_col_normal = (Body.just_collision_normal dt b ( b')) in just_col_normal <> Vec.Zero && Vec.abs_angle_deg just_col_normal Vec.up < fix64 90  )
    static member floor_velocity dt bs b' = Body.floor dt bs b' |> List.sumBy (fun b -> b.velocity)
    static member is_on_floor dt bs b' = Body.floor dt bs b' |> List.isEmpty|> not 
    static member move_and_slide_with dt bs fs b' = Body.translated dt {b' with velocity = Body.floor_velocity dt fs b' + (b'.velocity.slide <| Body.just_collisions_normal dt bs b')}
    static member place dt bs = until (fun x -> Body.collisions_normal bs x = Vec.Zero) (fun b -> {b with position = b.position + (Vec.scaled (fix64 1) (Body.collisions_normal bs b) (*|>| fun x -> GD.Print ("escape ", x)*) )})
    static member place_inside (v1:Vec) (v2:Vec) b = {b with position = Vec(clamp v1.x v2.x b.position.x, clamp v1.y v2.y b.position.y) }
let inline body a = (^a : (member body: Body)(a)) 
type BodyContainer = {body:Body}
let body_container b = {body = b}
let inline name a = (^a : (member name: string)(a)) 
let inline position a = (^a : (member position: Vec)(a))
let inline body_position a = (body >> position) a
let generic_walls = 
    [
        Body.basic -20 -20 (Rect(fix64 50, fix64 2000))
        Body.basic -20 -20 (Rect(fix64 2000, fix64 50))
        Body.basic 1950 1100 (Rect(fix64 50, fix64 2000))
        Body.basic 1950 1100 (Rect(fix64 2000, fix64 50))
    ]
module Body =
    let inline velocity (b:Body) = b.velocity
