let samplesLoc = "../../samples/" and samplesNb = [|21|]

let generateFileNames loc version nb =
  List.init nb (fun i -> let si = (string_of_int i) in
                 let si = (String.make (2 - (String.length si)) '0')^si in
                 Printf.sprintf "%sprog%s%s.aps" loc (string_of_int version) si)

let testParse f =
  if Sys.command (Printf.sprintf "./src/aps.exe -parse %s > /dev/null" f) <> 0
  then failwith (Printf.sprintf "Parse failed for file %s" f)

let testType f =
  if Sys.command (Printf.sprintf "./src/aps.exe -type %s > /dev/null " f) <> 0
  then failwith (Printf.sprintf "Typing failed for file %s" f)

let testEval f res =
  let ic = Unix.open_process_in (Printf.sprintf "./src/aps.exe -eval %s" f) in
  try (
    if (input_line ic) <> res
    then failwith (Printf.sprintf "Typing failed for file %s" f)
  ) with _ -> failwith (Printf.sprintf "Typing failed for file %s" f)

let generateAndTest loc version nb =
  let files = generateFileNames loc version nb in
  List.iter testParse files;
  Printf.printf "Tested parse for %s successfully !\n" loc;
  List.iter testType files;
  Printf.printf "Tested typing for %s successfully !\n" loc;
  let res = open_in (Printf.sprintf "%s/res%s.txt" loc (string_of_int version)) in
  List.iter (fun f -> testEval f (input_line res)) files

let _ =
  generateAndTest samplesLoc 0 samplesNb.(0)
