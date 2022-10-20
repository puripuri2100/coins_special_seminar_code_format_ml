open OUnit2
open Code_format_ml


let oneline_comment_format s = "// "^ s

let block_comment_format _ lst =
  let v = List.append lst ["*/"] in
  let v = "/*" :: v in
  v


type test =
  | A of int
  | B of float
  | AWithComment of string list * int * string option
  | BWithComment of string list * float * string option
  | AorB of test
  | C of test list
  | D of string list * test list
  | Let of string list * string * test


let make_rule_with_comment_none (rule:Base.rule) : Base.rule_with_comment =
  {
    before_comments = [];
    rule = rule;
    after_comment = None;
  }


let make_rule_with_comment (before_comments:string list) (rule:Base.rule) (after_comment:string option) : Base.rule_with_comment =
  {
    before_comments = before_comments;
    rule = rule;
    after_comment = after_comment;
  }


  let rec test_to_rule (test:test) : Base.rule_with_comment =
    match test with
    | A(i) -> make_rule_with_comment_none (Raw(string_of_int i))
    | B(f) -> make_rule_with_comment_none (Paren("(", make_rule_with_comment_none (Raw(string_of_float f)), ")"))
    | AWithComment(before_comments, i, after_comment) -> make_rule_with_comment before_comments (Raw(string_of_int i)) after_comment
    | BWithComment(before_comments, f, after_comment) -> make_rule_with_comment before_comments (
        Paren("(", make_rule_with_comment_none (Raw(string_of_float f)), ")")
      ) after_comment
    | AorB(t) -> (
      let ast = test_to_rule t in
      let rule = make_rule_with_comment_none (AST(ast)) in
      make_rule_with_comment_none (Paren("<", rule, ">"))
    )
    | C(lst) -> make_rule_with_comment_none (Paren("[", make_rule_with_comment_none (List(",", List.map test_to_rule lst)), "]"))
    | D(before_comments, lst) -> make_rule_with_comment before_comments (
        Paren("[", make_rule_with_comment_none (List(",", List.map test_to_rule lst)), "]")
      ) None
    | Let(before_comments, name, inner) -> (
      let default_cc:Base.column_config = { is_break = None; space_size = None; } in
      make_rule_with_comment before_comments (Column([
        (
          make_rule_with_comment_none (Raw("let")),
          default_cc |> Base.set_is_break (Some(false))
        );
        (
          make_rule_with_comment_none (Raw(name)),
          default_cc |> Base.set_is_break (Some(false)) |> Base.set_space_size 2
        );
        (
          make_rule_with_comment_none (Raw("=")),
          default_cc
        );
        (
          make_rule_with_comment_none (Paren("{", make_rule_with_comment_none (AST(test_to_rule inner)), "}")),
          default_cc |> Base.set_is_break (Some(false))
        );
      ])) None
    )


let test_code_format (rule:Base.rule_with_comment) : string = Lib.code_format oneline_comment_format block_comment_format rule

let tests = "test suite for code_format_ml" >::: [
  "check_generate_indent" >:: (fun _ ->
      assert_equal "    " (Base.str_repeat " " 4)
    );

  "check_lst_join" >:: (fun _ ->
      assert_equal "a,b,c" (Base.lst_join "," ["a"; "b"; "c"])
    );

  "check1" >:: (fun _ -> (
    let test = B(3.14) in
    let ok_str = "(3.14)" in
    assert_equal ok_str (test |> test_to_rule |> test_code_format)
  ));

  "check2" >:: (fun _ -> (
    let test = make_rule_with_comment_none (AST(
      make_rule_with_comment_none (Paren("<", make_rule_with_comment_none (Raw(string_of_int 42)), ">"))
    )) in
    let ok_str = "<42>" in
    assert_equal ok_str (test |> test_code_format)
  ));

  "check3" >:: (fun _ -> (
    let test = AorB(A(42)) in
    let ok_str = "<42>" in
    assert_equal ok_str (test |> test_to_rule |> test_code_format)
  ));

  "check4" >:: (fun _ -> (
    let test = C([AorB(A(42)); C([A(42); B(3.14)])]) in
    let ok_str = "[<42>, [42, (3.14)]]" in
    assert_equal ok_str (test |> test_to_rule |> test_code_format)
  ));

  "check5" >:: (fun _ -> (
    let test = C([
        AorB(A(42));
        C([A(42); B(3.14); B(3.141)]);
        AorB(A(3333333));
        A(3333333);
        AorB(A(3333333))
      ])
    in
    let ok_str = "[
  <42>,
  [42, (3.14), (3.141)],
  <3333333>,
  3333333,
  <3333333>
]"
    in
    assert_equal ok_str (test |> test_to_rule |> test_code_format)
  ));

  "check6" >:: (fun _ -> (
    let test = C([
      AorB(A(42));
      C([
        A(33333333333);
        B(33333.144444);
        B(33333.141111)
      ]);
      AorB(A(3333333));
      A(3333333);
      AorB(A(3333333))
    ])
    in
    let ok_str = "[
  <42>,
  [
    33333333333,
    (33333.144444),
    (33333.141111)
  ],
  <3333333>,
  3333333,
  <3333333>
]"
    in
    assert_equal ok_str (test |> test_to_rule |> test_code_format)
  ));

  "check7" >:: (fun _ -> (
    let test = D(
      ["hoge"; "fuga"],
      [
        AorB(A(42));
        C([
          A(33333333333);
          B(33333.144444);
          B(33333.141111)
        ]);
        AorB(A(3333333));
        A(3333333);
        AorB(A(3333333));
        C([
          AWithComment(["hoge"], 333333, Some("fuga"));
          BWithComment(["hoge"], 333.14, Some("fuga"));
          B(33333.141111)
        ])
      ])
    in
    let ok_str = "/*
hoge
fuga
*/
[
  <42>,
  [
    33333333333,
    (33333.144444),
    (33333.141111)
  ],
  <3333333>,
  3333333,
  <3333333>,
  [
    // hoge
    333333, // fuga
    // hoge
    (333.14), // fuga
    (33333.141111)
  ]
]"
    in
    assert_equal ok_str (test |> test_to_rule |> test_code_format)
  ));

  "check8" >:: (fun _ -> (
    let test = D(
      ["hoge"; "fuga"],
      [
        AorB(A(42));
        C([
          A(33333333333);
          B(33333.144444);
          B(33333.141111)
        ]);
        AorB(A(3333333));
        A(3333333);
        AorB(A(3333333));
        C([
          AWithComment(["hoge"], 333333, Some("fuga"));
          Let(
            ["短めのcolumnのテストです"],
            "name",
            A(3333333)
          );
          BWithComment(["hoge"], 333.14, Some("fuga"));
          B(33333.141111);
        ])
      ]
    )
    in
    let ok_str = "/*
hoge
fuga
*/
[
  <42>,
  [
    33333333333,
    (33333.144444),
    (33333.141111)
  ],
  <3333333>,
  3333333,
  <3333333>,
  [
    // hoge
    333333, // fuga
    // 短めのcolumnのテストです
    let name  = {3333333},
    // hoge
    (333.14), // fuga
    (33333.141111)
  ]
]"
    in
    assert_equal ok_str (test |> test_to_rule |> test_code_format)
  ));
]

let _ = run_test_tt_main tests
