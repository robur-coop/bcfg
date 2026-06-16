(* Copyright (c) 2024 The jsont programmers
   SPDX-License-Identifier: ISC
   Copyright (c) 2026 Romain Calascibetta <romain.calascibetta@gmail.com>
   SPDX-License-Identifier: ISC *)

module S = Map.Make (String)
module I = Map.Make (Int)

(* {1 Heterogeneous map keyed by type witnesses.}

   Used to collect the decoded value of each parameter/field, regardless of the
   order in which they appear, and apply them to the record constructor. *)
module Type = struct
  type ('a, 'b) refl = Refl : ('a, 'a) refl

  module Id = struct
    type _ t = ..

    module type S = sig
      type s
      type _ t += T : s t
    end

    type 'a m = (module S with type s = 'a)

    let create (type a) () : a m =
      (module struct
        type s = a
        type _ t += T : s t
      end)

    let equal (type a b) ((module A) : a m) ((module B) : b m) :
        (a, b) refl option =
      match A.T with B.T -> Some Refl | _ -> None

    let uid (type a) ((module A) : a m) =
      Obj.Extension_constructor.(id (of_val A.T))
  end
end

module Hmap = struct
  type binding = Binding : 'a Type.Id.m * 'a -> binding
  type t = binding I.t

  let find : type a. a Type.Id.m -> t -> a =
   fun k t ->
    let (Binding (k', v)) = I.find (Type.Id.uid k) t in
    match Type.Id.equal k k' with Some Refl -> v | None -> raise Not_found

  let empty = I.empty

  let add : type a. a Type.Id.m -> a -> t -> t =
   fun witness value t ->
    I.add (Type.Id.uid witness) (Binding (witness, value)) t
end

(* {1 Applicative constructor.} The decoder collects values in an {!Hmap.t} and
   applies them, in declaration order, to the record constructor. *)
type ('r, 'fn) decoder =
  | Fun : 'fn -> ('ret, 'fn) decoder
  | App : ('ret, 'a -> 'b) decoder * 'a Type.Id.m -> ('ret, 'b) decoder

let rec apply : type r fn. (r, fn) decoder -> Hmap.t -> fn =
 fun decoder ctx ->
  match decoder with
  | Fun fn -> fn
  | App (fn, arg) -> apply fn ctx (Hmap.find arg ctx)

type error = [ `Msg of string ]

let error fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind

let rec sequence = function
  | [] -> Ok []
  | x :: r ->
      let* v = x in
      let* rest = sequence r in
      Ok (v :: rest)

type 'a t =
  | Scalar : 'a scalar -> 'a t
  | Obj : ('a, 'a) directive -> 'a t
  | Map : ('a -> ('b, error) result) * ('b -> 'a) * 'a t -> 'b t
  | List : 'a t -> 'a list t
  | Option : 'a t -> 'a option t
  | Rec : 'a t Lazy.t -> 'a t
  | Cases : ('a, 'tag) cases -> 'a t

and 'a scalar = { sdec : string -> ('a, error) result; senc : 'a -> string }

and ('a, 'tag) cases = {
  cname : string option;
  ctag : string;
  ctagtype : 'tag t;
  ccases : ('a, 'tag) case list;
}

and ('a, 'tag) case =
  | Case : {
      tag : 'tag;
      cdir : ('v, 'v) directive;
      inject : 'v -> 'a;
      project : 'a -> 'v option;
    }
      -> ('a, 'tag) case

and ('r, 'fn) directive = {
  dname : string option;
  ddoc : string option;
  iparams : 'r pfield I.t;
  fields : 'r ffield S.t;
  forder : string list;
  decoder : ('r, 'fn) decoder;
}

and 'r pfield = Pfield : ('a, 'r) param -> 'r pfield

and ('a, 'r) param = {
  ptype : 'a t;
  pwit : 'a Type.Id.m;
  pget : 'r -> 'a;
  pdoc : string option;
}

and 'r ffield = Ffield : ('a, 'r) field -> 'r ffield

and ('a, 'r) field = {
  fname : string;
  ftype : 'a t;
  fwit : 'a Type.Id.m;
  fget : 'r -> 'a;
  fdoc : string option;
}

(* {2 Scalars.} *)

let string = Scalar { sdec = (fun s -> Ok s); senc = Fun.id }

let int =
  Scalar
    {
      sdec =
        (fun s ->
          match int_of_string_opt s with
          | Some i -> Ok i
          | None -> error "%S is not a valid integer" s);
      senc = string_of_int;
    }

let bool =
  Scalar
    {
      sdec =
        (fun s ->
          match bool_of_string_opt s with
          | Some b -> Ok b
          | None -> error "%S is not a valid boolean" s);
      senc = string_of_bool;
    }

let float =
  Scalar
    {
      sdec =
        (fun s ->
          match float_of_string_opt s with
          | Some f -> Ok f
          | None -> error "%S is not a valid float" s);
      senc = string_of_float;
    }

let map ~dec ~enc t = Map ((fun a -> Ok (dec a)), enc, t)
let list t = List t
let option t = Option t

let enum cases =
  let dec s =
    match List.assoc_opt s cases with
    | Some v -> Ok v
    | None ->
        error "%S is not one of: %s" s (String.concat ", " (List.map fst cases))
  in
  let enc v =
    match List.find_opt (fun (_, v') -> v' = v) cases with
    | Some (s, _) -> s
    | None -> invalid_arg "Bcfgt.enum: value has no encoding"
  in
  Map (dec, enc, string)

let fix fn =
  let rec t = lazy (fn (Rec t)) in
  Rec t

(* {2 Directive builders.} *)

let directive ?name:dname ?documentation:ddoc fn =
  {
    dname;
    ddoc;
    iparams = I.empty;
    fields = S.empty;
    forder = [];
    decoder = Fun fn;
  }

let req ?pos ?documentation:pdoc ptype pget directive =
  let idx =
    match pos with Some p -> p | None -> I.cardinal directive.iparams
  in
  let pwit = Type.Id.create () in
  let param = Pfield { ptype; pwit; pget; pdoc } in
  {
    directive with
    iparams = I.add idx param directive.iparams;
    decoder = App (directive.decoder, pwit);
  }

let field fname ?documentation:fdoc ftype fget directive =
  let fwit = Type.Id.create () in
  let field = Ffield { fname; ftype; fwit; fget; fdoc } in
  {
    directive with
    fields = S.add fname field directive.fields;
    forder = fname :: directive.forder;
    decoder = App (directive.decoder, fwit);
  }

let opt fname ?documentation:fdoc ftype ?get directive =
  let fget = match get with Some g -> g | None -> Fun.const None in
  let fwit = Type.Id.create () in
  let field = Ffield { fname; ftype = Option ftype; fwit; fget; fdoc } in
  {
    directive with
    fields = S.add fname field directive.fields;
    forder = fname :: directive.forder;
    decoder = App (directive.decoder, fwit);
  }

let some directive = List (Obj directive)
let uniq directive = Obj directive
let case tag cdir ~inject ~project = Case { tag; cdir; inject; project }

let cases ?name:cname ~tag:ctag ctagtype ccases =
  Cases { cname; ctag; ctagtype; ccases }

(* {2 Decoding.} *)

let rec top_name : type a. a t -> string option = function
  | Obj { dname; _ } -> dname
  | Cases { cname; _ } -> cname
  | Map (_, _, inner) -> top_name inner
  | List inner -> top_name inner
  | Option inner -> top_name inner
  | Rec l -> top_name (Lazy.force l)
  | Scalar _ -> None

let rec decode_string : type a. a t -> string -> (a, error) result =
 fun t str ->
  match t with
  | Scalar s -> s.sdec str
  | Map (f, _, inner) -> Result.bind (decode_string inner str) f
  | Rec l -> decode_string (Lazy.force l) str
  | Obj _ | List _ | Option _ | Cases _ ->
      error "expected a scalar value, got a directive"

let rec decode_in : type a. a t -> Bcfg.directive -> (a, error) result =
 fun t d ->
  match t with
  | Scalar s -> (
      match d.Bcfg.parameters with
      | p :: _ -> s.sdec p
      | [] -> error "directive %S expects a parameter" d.Bcfg.name)
  | Obj dir -> decode_directive dir d
  | Cases c -> decode_cases c d
  | Map (f, _, inner) -> Result.bind (decode_in inner d) f
  | Rec l -> decode_in (Lazy.force l) d
  | List _ | Option _ ->
      error "unexpected list/option for directive %S" d.Bcfg.name

and decode_cases : type a tag.
    (a, tag) cases -> Bcfg.directive -> (a, error) result =
 fun c d ->
  let matches = List.filter (fun ch -> ch.Bcfg.name = c.ctag) d.Bcfg.children in
  let* tag =
    match matches with
    | [ td ] -> decode_in c.ctagtype td
    | [] -> error "missing discriminator field %S" c.ctag
    | _ -> error "discriminator field %S appears more than once" c.ctag
  in
  let rec pick = function
    | [] -> error "no case matches the discriminator value of %S" c.ctag
    | Case cs :: rest ->
        if cs.tag = tag then
          let* v = decode_directive cs.cdir d in
          Ok (cs.inject v)
        else pick rest
  in
  pick c.ccases

and decode_param : type a. a t -> string option -> (a, error) result =
 fun t str ->
  match (t, str) with
  | Option _, None -> Ok None
  | Option inner, Some s ->
      let* v = decode_string inner s in
      Ok (Some v)
  | _, Some s -> decode_string t s
  | _, None -> error "missing positional parameter"

and decode_field : type a.
    a t -> string -> Bcfg.directive list -> (a, error) result =
 fun t name matches ->
  match t with
  | Option inner -> (
      match matches with
      | [] -> Ok None
      | [ d ] ->
          let* v = decode_in inner d in
          Ok (Some v)
      | _ -> error "field %S appears more than once" name)
  | List inner -> sequence (List.map (decode_in inner) matches)
  | other -> (
      match matches with
      | [ d ] -> decode_in other d
      | [] -> error "missing field %S" name
      | _ -> error "field %S appears more than once" name)

and decode_directive : type r.
    (r, r) directive -> Bcfg.directive -> (r, error) result =
 fun dir d ->
  let* hmap =
    I.fold
      (fun idx (Pfield p) acc ->
        let* hmap = acc in
        let* v = decode_param p.ptype (List.nth_opt d.Bcfg.parameters idx) in
        Ok (Hmap.add p.pwit v hmap))
      dir.iparams (Ok Hmap.empty)
  in
  let* hmap =
    S.fold
      (fun fname (Ffield f) acc ->
        let* hmap = acc in
        let matches =
          List.filter (fun c -> c.Bcfg.name = fname) d.Bcfg.children
        in
        let* v = decode_field f.ftype fname matches in
        Ok (Hmap.add f.fwit v hmap))
      dir.fields (Ok hmap)
  in
  Ok (apply dir.decoder hmap)

let rec decode_top : type a. a t -> Bcfg.t -> (a, error) result =
 fun t ds ->
  match t with
  | Obj dir -> begin
      let ds =
        match dir.dname with
        | Some n -> List.filter (fun d -> d.Bcfg.name = n) ds
        | None -> ds
      in
      match ds with
      | [ d ] -> decode_directive dir d
      | [] -> error "no matching directive"
      | _ -> error "expected exactly one directive"
    end
  | Cases c -> begin
      let ds =
        match c.cname with
        | Some n -> List.filter (fun d -> d.Bcfg.name = n) ds
        | None -> ds
      in
      match ds with
      | [ d ] -> decode_cases c d
      | [] -> error "no matching directive"
      | _ -> error "expected exactly one directive"
    end
  | List inner ->
      let ds =
        match top_name inner with
        | Some n -> List.filter (fun d -> d.Bcfg.name = n) ds
        | None -> ds
      in
      sequence (List.map (decode_in inner) ds)
  | Option inner -> begin
      let ds =
        match top_name inner with
        | Some n -> List.filter (fun d -> d.Bcfg.name = n) ds
        | None -> ds
      in
      match ds with
      | [] -> Ok None
      | [ d ] ->
          let* v = decode_in inner d in
          Ok (Some v)
      | _ -> error "expected at most one directive"
    end
  | Map (f, _, inner) -> Result.bind (decode_top inner ds) f
  | Rec l -> decode_top (Lazy.force l) ds
  | Scalar _ -> error "cannot decode a scalar at the top-level"

let decode t ds =
  match decode_top t ds with Ok v -> Ok v | Error (`Msg _ as e) -> Error e

(* {2 Encoding.} *)

let rec enc_scalar : type a. a t -> a -> string =
 fun t v ->
  match t with
  | Scalar s -> s.senc v
  | Map (_, bwd, inner) -> enc_scalar inner (bwd v)
  | Rec l -> enc_scalar (Lazy.force l) v
  | Obj _ | List _ | Option _ | Cases _ ->
      invalid_arg "Bcfgt.encode: a positional parameter must be a scalar"

let rec enc_dir : type a. a t -> string -> a -> Bcfg.directive =
 fun t name v ->
  match t with
  | Scalar s -> { Bcfg.name; parameters = [ s.senc v ]; children = [] }
  | Obj dir -> encode_named dir name v
  | Cases c -> encode_cases c name v
  | Map (_, bwd, inner) -> enc_dir inner name (bwd v)
  | Rec l -> enc_dir (Lazy.force l) name v
  | List _ | Option _ ->
      invalid_arg "Bcfgt.encode: nested list/option handled at field level"

and encode_cases : type a tag. (a, tag) cases -> string -> a -> Bcfg.directive =
 fun c name v ->
  let rec pick = function
    | [] -> invalid_arg "Bcfgt.encode: the value matches no case"
    | Case cs :: rest ->
        begin match cs.project v with
        | Some payload ->
            let d = encode_named cs.cdir name payload in
            let tag_child = enc_dir c.ctagtype c.ctag cs.tag in
            { d with Bcfg.children = tag_child :: d.Bcfg.children }
        | None -> pick rest
        end
  in
  pick c.ccases

and encode_one_field : type r. r ffield -> r -> Bcfg.directive list =
 fun (Ffield f) v ->
  match f.ftype with
  | Option inner -> (
      match f.fget v with None -> [] | Some x -> [ enc_dir inner f.fname x ])
  | List inner -> List.map (enc_dir inner f.fname) (f.fget v)
  | other -> [ enc_dir other f.fname (f.fget v) ]

and encode_one_param : type r. r pfield -> r -> string list =
 fun (Pfield p) v ->
  match p.ptype with
  | Option inner -> (
      match p.pget v with None -> [] | Some x -> [ enc_scalar inner x ])
  | other -> [ enc_scalar other (p.pget v) ]

and encode_named : type r. (r, r) directive -> string -> r -> Bcfg.directive =
 fun dir name v ->
  let parameters =
    I.bindings dir.iparams
    |> List.concat_map (fun (_, pfield) -> encode_one_param pfield v)
  in
  let children =
    List.rev dir.forder
    |> List.concat_map (fun fname ->
        encode_one_field (S.find fname dir.fields) v)
  in
  { Bcfg.name; parameters; children }

let rec encode : type a. a t -> a -> Bcfg.t =
 fun t v ->
  match t with
  | Obj dir ->
      let name =
        match dir.dname with
        | Some n -> n
        | None -> invalid_arg "Bcfgt.encode: top-level directive without a name"
      in
      [ encode_named dir name v ]
  | Cases c ->
      let name =
        match c.cname with
        | Some n -> n
        | None -> invalid_arg "Bcfgt.encode: top-level cases without a name"
      in
      [ encode_cases c name v ]
  | List inner -> List.concat_map (encode inner) v
  | Option inner -> ( match v with None -> [] | Some x -> encode inner x)
  | Map (_, bwd, inner) -> encode inner (bwd v)
  | Rec l -> encode (Lazy.force l) v
  | Scalar s -> [ { Bcfg.name = s.senc v; parameters = []; children = [] } ]
