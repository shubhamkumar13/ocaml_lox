module Scanner = struct
        let range initial final =
                let rec range' = fun acc initial final ->
                        match initial < final with
                        | false -> acc
                        | true -> range' (initial :: acc) (initial + 1) final in
                range' [] initial final
        let scan_tokens cmd =
                List.map (fun x -> cmd.[x]) (range 0 (String.length cmd - 1))
                |> List.filter (fun x -> x != ' ') 
end

let input = Sys.argv

let input_length = Array.length @@ Sys.argv

let file_to_stringlist (file_name : string) : string list =
        let lines = ref [] in
        let chan = open_in file_name in
        try
                while true; do
                lines := input_line chan :: !lines
                done; []
        with End_of_file ->
                close_in chan;
                List.rev !lines


let run (cmd : string) =
        Scanner.scan_tokens cmd
        

let run_file file_name = 
        let file_to_string = List.fold_left (fun acc x -> acc ^ "\n" ^ x) "" @@ file_to_stringlist file_name in
        run file_to_string |> List.iter (fun x -> Printf.printf "%c\n" x)

let run_prompt () =
        try
                while true do
                Printf.printf "> ";
                let input_line = read_line () in
                run input_line;
                done; ()
        with End_of_file ->
                Printf.printf "Bye!\n"

let main () =
        match input_length with
        | 1 -> Printf.printf "%d\n" input_length; run_prompt ()
        | 2 -> Printf.printf "%d\n" input_length; run_file input.(1)
        | _ -> failwith "Usage: caml_lox [script]"
        

let _ = main ()