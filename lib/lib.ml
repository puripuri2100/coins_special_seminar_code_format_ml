open Code_format
open Base


let code_format oneline_comment_format block_comment_format rule_with_comment =
  let ctx = {
    depth = 0;
    tab_spaces = 2;
    line_width = 35;
    break_str = "\n";
    list_join_str = None;
    oneline_comment_format = oneline_comment_format;
    block_comment_format = block_comment_format;
  } in
  let (code_str_lst, _) = code_format ctx rule_with_comment in
  lst_join ctx.break_str code_str_lst

