open! Core
open! Stdio

module Repl = struct
  open! Lexer

  let process_line line =
    MonkeyLexer.tokenize line
    |> sexp_of_list sexp_of_monkey_tokens
    |> Sexp.to_string

  let rec run_repl in_c out_c =
    let read_line = In_channel.input_line in_c in
    let write_string x =
      let _ = Out_channel.output_string out_c x in
      let _ = Out_channel.newline out_c in
      Out_channel.flush out_c
    in
    match read_line with
    | None -> run_repl in_c out_c
    | Some "exit" -> ()
    | Some read_line ->
        let _ = process_line read_line |> write_string in
        run_repl in_c out_c
end

let () =
  let _ = print_endline "This is the Monkey programming language REPL." in
  let _ =
    print_endline "You can type commands below. Type 'exit' to exit the REPL"
  in
  Repl.run_repl In_channel.stdin Out_channel.stdout
