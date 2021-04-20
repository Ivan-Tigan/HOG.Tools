module HOG.Tools.BinaryWR
open System.IO
open FSharpPlus.Internals
open Serializers
open FixMath.NET

type BinaryWR<'a> = {write: BinaryWriter -> 'a -> unit; read: BinaryReader -> 'a}
    let mk_binary_serializer<'o> (s:'o -> BinaryWriter -> unit) d = {
        serialize = fun (o:'o) -> let stream = new MemoryStream() in let writer = new BinaryWriter(stream) in let _ = s o writer in stream.ToArray();
        deserialize = fun bs -> d (new BinaryReader(new MemoryStream(bs))) }
    let serialize_sized (w:BinaryWriter) (o_ser:'o -> byte []) (o) = let ser = o_ser o in let _ = w.Write(ser.Length) in w.Write (ser) 
    let mk_binary_wr w r = {write = w; read = r} 
    let bin_ser_from_wr wr = mk_binary_serializer (fun o w -> wr.write w o) (fun r -> wr.read r)
    let compress (bs:byte[]) (d:int -> unit) = Ionic.Zlib.ZlibStream.CompressBuffer bs
    let decompress (bs:byte []) = Ionic.Zlib.ZlibStream.UncompressBuffer bs
    let (.>>.) b1 b2 = {write = (fun w (o1, o2) -> let _ = b1.write w o1 in let _ = b2.write w o2 in ()); read = fun r -> b1.read r, b2.read r}
    let bwr_tuple a b = a .>>. b 
    let bwr_bind fw fr b = {write = (fun w o -> let _ = b.write w o in (fw o).write w o); read = (fun r -> b.read r |> fun x -> (fr x).read r)}
//        let bwr_choice c = {write = (fun w o -> (c o).write); read ->  }
    let bwr_string = mk_binary_wr (fun w (o:string) -> w.Write o ) (fun r -> r.ReadString())
    let bwr_many b = mk_binary_wr (fun w (os:'a list) -> let _ = w.Write (int32 os.Length) in for o in os do b.write w o ) (fun r -> List.init (r.ReadInt32()) (fun _ -> b.read r))
    let bwr_hashmap k v = let kv = bwr_tuple k v in mk_binary_wr (fun w (os:Map<'key, 'value>) -> let _ = w.Write (int32 os.Count) in for o in Map.toList os do kv.write w o ) (fun r -> List.init (r.ReadInt32()) (fun _ -> kv.read r) |> Map.ofList)
    let bwr_option b = mk_binary_wr (fun w (o:'a option) -> (match o with  None -> w.Write false | Some o -> let _ = w.Write true in b.write w o)) (fun r -> match (r.ReadBoolean()) with false -> None | true -> Some (b.read r))
    let bwr_either left right = mk_binary_wr (fun w (o:Either<'a,'b>) -> (match o with Left a -> let _ = w.Write false in left.write w a | Right o -> let _ = w.Write true in right.write w o)) (fun r -> match (r.ReadBoolean()) with false -> Left(left.read r) | true -> Right (right.read r))
    let bwr_char = mk_binary_wr (fun w (o:char) -> w.Write o ) (fun r -> r.ReadChar())
    let bwr_byte = mk_binary_wr (fun w (o:byte) -> w.Write o ) (fun r -> r.ReadByte())
    let bwr_sbyte = mk_binary_wr (fun w (o:sbyte) -> w.Write o ) (fun r -> r.ReadSByte())
    let bwr_int64 = mk_binary_wr (fun w (o:int64) -> w.Write o ) (fun r -> r.ReadInt64())
    let bwr_bool = mk_binary_wr (fun w (o:bool) -> w.Write o ) (fun r -> r.ReadBoolean())
    let bwr_kchar (k:char) = mk_binary_wr (fun w o -> w.Write k ) (fun r -> r.ReadChar())
    let bwr_kint64 (k:int64) = mk_binary_wr (fun w o -> w.Write k ) (fun r -> r.ReadInt64())
    let bwr_kbool (k:bool) = mk_binary_wr (fun w o -> w.Write k ) (fun r -> r.ReadBoolean())
    let bwr_json<'a> =
        mk_binary_wr
            (fun w o -> let ser = pickle_serializer<'a>.serialize o in let _ = w.Write (uint64 ser.Length) in w.Write ser)
            (fun r -> let l = r.ReadInt64() in r.ReadBytes(int l) |> pickle_serializer<'a>.deserialize)
    let bwr_map enter exit b1 = {write = (fun w o -> enter o |> b1.write w ); read = (fun r -> b1.read r |> exit)}
