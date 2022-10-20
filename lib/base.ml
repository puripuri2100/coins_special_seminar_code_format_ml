
type column_config = {
  is_break : bool option;
  space_size : int option;
}

let set_is_break is_break_opt self = {self with is_break = is_break_opt}
let set_space_size size self = {self with space_size = Some(size)}

type rule =
  | AST of rule_with_comment
  | Raw of string
  | List of string * rule_with_comment list
  | Paren of string * rule_with_comment * string
  | Column of (rule_with_comment * column_config) list

and rule_with_comment = {
  before_comments : string list;
  rule : rule;
  after_comment : string option;
}


type context = {
  depth : int;
  tab_spaces: int;
  line_width: int;
  break_str: string;
  list_join_str: string option;
  oneline_comment_format: string -> string;
  block_comment_format: context -> string list -> string list;
}


let str_repeat str count =
  let rec sub count buf =
    if count == 0 then
      buf
    else
      sub (count - 1) (buf ^ str)
  in
  sub count ""


let increment_depth self = { self with depth = self.depth + 1 }
let indent self = str_repeat " " self.tab_spaces
let len_max self =
  let indent_len = self.tab_spaces * self.depth in
  self.line_width - indent_len
let set_list_join_str j_opt self = { self with list_join_str = j_opt }


let is_string_empty str = String.equal str ""

let is_lst_empty lst =
  match lst with
  | [] -> true
  | _ -> false


let lst_join join lst =
  let rec sub lst buf =
    match lst with
    | [] -> buf
    | x::[] -> if is_string_empty buf then x else buf ^ join ^ x
    | x::xs -> if is_string_empty buf then sub xs x else sub xs (buf ^ join ^ x)
  in
  sub lst ""


let option_unwrap_or value opt =
  match opt with
  | Some(v) -> v
  | None -> value
