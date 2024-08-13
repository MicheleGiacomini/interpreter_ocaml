open! Core
open Ppx_compare_lib.Builtin

type monkey_tokens =
  (*Special*)
  | Illegal
  | EOF
  (*Identifiers, literals*)
  | Identifier of string
  | Int of int
  | String of string
  (*Operators*)
  | Assign
  | Plus
  | Minus
  | Slash
  | Asterisk
  | Bang
  | Lt
  | Gt
  | Eq
  | Not_eq
  (*Delimiters*)
  | Comma
  | Semicolon
  | Colon
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Lbrace
  | Rbrace
  (*Keywords*)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
[@@deriving compare, sexp]

module MonkeyLexer = struct
  type lexer_state = { input : string; read_position : int; ch : char }

  let is_identifier_first_character = Char.is_alpha

  let is_identifier_character ch =
    let open Char in
    is_alphanum ch || '_' = ch

  let is_digit = Char.is_digit

  (** Match any space or new-line character *)
  let is_space_character ch =
    let open Char in
    is_whitespace ch && ch <> '\x00'

  let advance_state current_state =
    let module S = String in
    match current_state.input with
    | "" ->
        {
          input = current_state.input;
          read_position = current_state.read_position;
          ch = '\x00';
        }
    | _ ->
        {
          input = S.drop_prefix current_state.input 1;
          read_position = current_state.read_position + 1;
          ch = S.get current_state.input 0;
        }

  (** Equivalent of take_while, returns the state pointing at the first character where the predicate is false *)
  let read_while predicate current_state =
    let module S = String in
    let rec internal acc current_state =
      if predicate current_state.ch then
        internal
          (acc ^ Char.to_string current_state.ch)
          (advance_state current_state)
      else (acc, current_state)
    in
    let read, state = internal "" current_state in
    match read with "" -> (None, state) | _ -> (Some read, state)

  (** Read an identifier, return the resulting sting and the state on the first character AFTER the identifier *)
  let read_identifier current_state =
    if is_identifier_first_character current_state.ch then
      let tail, state =
        read_while is_identifier_character (advance_state current_state)
      in
      match tail with
      | None -> (Some (Char.to_string current_state.ch), state)
      | Some tail -> (Some (Char.to_string current_state.ch ^ tail), state)
    else (None, current_state)

  let read_integer current_state = read_while is_digit current_state

  let read_string current_state =
    let open Char in
    if current_state.ch = '"' then
      let str, state =
        read_while (fun x -> x <> '"') (advance_state current_state)
      in
      match str with
      | None -> (Some "", advance_state state)
      | Some str -> (Some str, advance_state state)
    else (None, current_state)

  (** consume zero or more space characters and return the state pointing at the first non-space found *)
  let consume_space current_state =
    let _, state = read_while is_space_character current_state in
    state

  let init_state input =
    { input; read_position = 0; ch = '\x00' } |> advance_state |> consume_space

  (** Read the first token that appears in the state and advance the state to the first non-space character following the token *)
  let next_token current_state =
    match current_state.ch with
    | '\x00' -> (EOF, consume_space (advance_state current_state))
    | '+' -> (Plus, consume_space (advance_state current_state))
    | '-' -> (Minus, consume_space (advance_state current_state))
    | '*' -> (Asterisk, consume_space (advance_state current_state))
    | '/' -> (Slash, consume_space (advance_state current_state))
    | '(' -> (Lparen, consume_space (advance_state current_state))
    | ')' -> (Rparen, consume_space (advance_state current_state))
    | '[' -> (Lbracket, consume_space (advance_state current_state))
    | ']' -> (Rbracket, consume_space (advance_state current_state))
    | '<' -> (Lt, consume_space (advance_state current_state))
    | '>' -> (Gt, consume_space (advance_state current_state))
    | '{' -> (Lbrace, consume_space (advance_state current_state))
    | '}' -> (Rbrace, consume_space (advance_state current_state))
    | ',' -> (Comma, consume_space (advance_state current_state))
    | ';' -> (Semicolon, consume_space (advance_state current_state))
    | ':' -> (Colon, consume_space (advance_state current_state))
    | '!' -> (
        let next = advance_state current_state in
        match next.ch with
        | '=' -> (Not_eq, consume_space (advance_state next))
        | _ -> (Bang, consume_space (advance_state current_state)))
    | '=' -> (
        let next = advance_state current_state in
        match next.ch with
        | '=' -> (Eq, consume_space (advance_state next))
        | _ -> (Assign, consume_space (advance_state current_state)))
    | _ -> (
        let identifier, advanced_state = read_identifier current_state in
        match identifier with
        | Some "let" -> (Let, consume_space advanced_state)
        | Some "fn" -> (Function, consume_space advanced_state)
        | Some "if" -> (If, consume_space advanced_state)
        | Some "else" -> (Else, consume_space advanced_state)
        | Some "return" -> (Return, consume_space advanced_state)
        | Some "true" -> (True, consume_space advanced_state)
        | Some "false" -> (False, consume_space advanced_state)
        | Some identifier ->
            (Identifier identifier, consume_space advanced_state)
        | None -> (
            let integer, advanced_state = read_integer current_state in
            match integer with
            | Some integer ->
                (Int (Int.of_string integer), consume_space advanced_state)
            | None -> (
                let str, advanced_state = read_string current_state in
                match str with
                | Some str -> (String str, consume_space advanced_state)
                | None ->
                    (Illegal, advanced_state |> advance_state |> consume_space))
            ))

  let tokenize input =
    let initial_state = init_state input in
    let rec tokens past_tokens current_state =
      let current_token = next_token current_state in
      match current_token with
      | EOF, _ -> EOF :: past_tokens
      | token, advanced_state -> tokens (token :: past_tokens) advanced_state
    in
    List.rev (tokens [] initial_state)
end

let%test_unit "symbol_tokens" =
  let open MonkeyLexer in
  [%test_result: monkey_tokens list] (tokenize "=+(){},;")
    ~expect:
      [ Assign; Plus; Lparen; Rparen; Lbrace; Rbrace; Comma; Semicolon; EOF ]

let%test_unit "simmple_program" =
  let open MonkeyLexer in
  [%test_result: monkey_tokens list]
    (tokenize
       {|
let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
|})
    ~expect:
      [
        Let;
        Identifier "five";
        Assign;
        Int 5;
        Semicolon;
        Let;
        Identifier "ten";
        Assign;
        Int 10;
        Semicolon;
        Let;
        Identifier "add";
        Assign;
        Function;
        Lparen;
        Identifier "x";
        Comma;
        Identifier "y";
        Rparen;
        Lbrace;
        Identifier "x";
        Plus;
        Identifier "y";
        Semicolon;
        Rbrace;
        Semicolon;
        Let;
        Identifier "result";
        Assign;
        Identifier "add";
        Lparen;
        Identifier "five";
        Comma;
        Identifier "ten";
        Rparen;
        Semicolon;
        EOF;
      ]

let%test_unit "full_token_set" =
  let open MonkeyLexer in
  [%test_result: monkey_tokens list]
    (tokenize
       {|let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
    |})
    ~expect:
      [
        Let;
        Identifier "five";
        Assign;
        Int 5;
        Semicolon;
        Let;
        Identifier "ten";
        Assign;
        Int 10;
        Semicolon;
        Let;
        Identifier "add";
        Assign;
        Function;
        Lparen;
        Identifier "x";
        Comma;
        Identifier "y";
        Rparen;
        Lbrace;
        Identifier "x";
        Plus;
        Identifier "y";
        Semicolon;
        Rbrace;
        Semicolon;
        Let;
        Identifier "result";
        Assign;
        Identifier "add";
        Lparen;
        Identifier "five";
        Comma;
        Identifier "ten";
        Rparen;
        Semicolon;
        Bang;
        Minus;
        Slash;
        Asterisk;
        Int 5;
        Semicolon;
        Int 5;
        Lt;
        Int 10;
        Gt;
        Int 5;
        Semicolon;
        If;
        Lparen;
        Int 5;
        Lt;
        Int 10;
        Rparen;
        Lbrace;
        Return;
        True;
        Semicolon;
        Rbrace;
        Else;
        Lbrace;
        Return;
        False;
        Semicolon;
        Rbrace;
        Int 10;
        Eq;
        Int 10;
        Semicolon;
        Int 10;
        Not_eq;
        Int 9;
        Semicolon;
        String "foobar";
        String "foo bar";
        Lbracket;
        Int 1;
        Comma;
        Int 2;
        Rbracket;
        Semicolon;
        Lbrace;
        String "foo";
        Colon;
        String "bar";
        Rbrace;
        EOF;
      ]
