type directive = {
  name : string;
  parameters : string list;
  children : directive list;
}

type t = directive list

module Stream = struct
  type lexeme = Ds of string | P of string | Os | Oe | De

  let pp ppf = function
    | Ds name -> Format.fprintf ppf "Ds %S" name
    | P p -> Format.fprintf ppf "P %S" p
    | Os -> Format.pp_print_string ppf "Os"
    | Oe -> Format.pp_print_string ppf "Oe"
    | De -> Format.pp_print_string ppf "De"
end
