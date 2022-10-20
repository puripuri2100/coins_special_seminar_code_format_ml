type column_config = {
  is_break : bool option;
  space_size : int option;
}


val set_is_break : bool option -> column_config -> column_config
val set_space_size : int -> column_config -> column_config

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


val str_repeat : string -> int -> string

val increment_depth : context -> context
val indent : context -> string
val len_max : context -> int
val set_list_join_str: string option -> context -> context

val lst_join : string -> string list -> string
val is_string_empty : string -> bool
val is_lst_empty : 'a list -> bool
val option_unwrap_or : 'a -> 'a option -> 'a

