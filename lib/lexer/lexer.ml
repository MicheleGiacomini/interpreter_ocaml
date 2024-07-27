open! Core
open Ppx_compare_lib.Builtin

type monkey_tokens =
  | Illegal
  | EOF
  | Identifier
  | Int
  | Assign
  | Plus
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Function
  | Let
[@@deriving compare, sexp]

type token = monkey_tokens * char [@@deriving compare, sexp]

module MonkeyLexer = struct
  type lexer_state = { input : string; read_position : int; ch : char }

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

  let init_state input = advance_state { input; read_position = 0; ch = '\x00' }

  let current_token current_state =
    match current_state.ch with
    | '\x00' -> (EOF, current_state.ch)
    | '=' -> (Assign, current_state.ch)
    | '+' -> (Plus, current_state.ch)
    | '(' -> (Lparen, current_state.ch)
    | ')' -> (Rparen, current_state.ch)
    | '{' -> (Lbrace, current_state.ch)
    | '}' -> (Rbrace, current_state.ch)
    | ',' -> (Comma, current_state.ch)
    | ';' -> (Semicolon, current_state.ch)
    | _ -> (Illegal, current_state.ch)

  let tokenize input =
    let initial_state = init_state input in
    let rec tokens past_tokens current_state =
      let current_token = current_token current_state in
      match current_token with
      | EOF, _ -> current_token :: past_tokens
      | _ -> tokens (current_token :: past_tokens) (advance_state current_state)
    in
    List.rev (tokens [] initial_state)
end

let%test_unit "symbol_tokens" =
  let open MonkeyLexer in
  [%test_result: token list] (tokenize "=+(){},;")
    ~expect:
      [
        (Assign, '=');
        (Plus, '+');
        (Lparen, '(');
        (Rparen, ')');
        (Lbrace, '{');
        (Rbrace, '}');
        (Comma, ',');
        (Semicolon, ';');
        (EOF, '\x00');
      ]
