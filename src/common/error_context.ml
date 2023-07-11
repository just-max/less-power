module One = struct

  type ('a, 'c) t = ('a, string list * 'c) result

  let ok : _ -> _ t = Result.ok

  let error ?msg e : _ t = Result.error (Option.to_list msg, e)

  let of_result ?msg r : _ t = Result.map_error (fun e -> Option.to_list msg, e) r

  let with_err_msg' msg : _ t -> _ t =
    Result.map_error (fun (msgs, c) -> msg () :: msgs, c)

  let with_err_msg msg = with_err_msg' (Fun.const msg)

  let map_cause f : _ t -> _ t = Result.map_error (fun (msgs, c) -> msgs, f c)
end

module Many = struct
  type ('a, 'c) t = ('a, 'c) One.t Seq.t

  let return x : _ t = Seq.return (Ok x)

  let map f (m : _ t) : _ t =
    Seq.map (Result.map f) m

  let join (mm : (_ t, _) t) : _ t =
    let join1 = function
      | Ok s -> s
      | Error c -> Seq.return (Error c)
    in
    Seq.concat_map join1 mm

  let bind m f = join (map f m)

  let of_one : _ One.t -> _ t = Seq.return

  let of_one_seq (s : (_ Seq.t, _) One.t) : _ t = bind (of_one s) (Seq.map One.ok)

  let of_seq s : _ t = Seq.map One.ok s
end
