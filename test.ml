let samplesLoc = "../../samples/" and samplesNb = [|21;21;9;7;2|]

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
    then failwith (Printf.sprintf "Evaluation failed for file %s" f)
  ) with _ -> failwith (Printf.sprintf "Evaluation failed for file %s" f)

let generateAndTest loc version nb =
  let files = generateFileNames loc version nb in
  List.iter testParse files;
  Printf.printf "Tested parse for version %d successfully !\n" version;
  List.iter testType files;
  Printf.printf "Tested typing for version %d successfully !\n" version;
  let res = open_in (Printf.sprintf "%s/res%s.txt" loc (string_of_int version)) in
  List.iter (fun f -> testEval f (input_line res)) files;
  Printf.printf "Tested evaluation for version %d successfully !\n" version

let _ =
  generateAndTest samplesLoc 0 samplesNb.(0);
  generateAndTest samplesLoc 1 samplesNb.(1);
  generateAndTest samplesLoc 2 samplesNb.(2);
  generateAndTest samplesLoc 3 samplesNb.(3);
  generateAndTest samplesLoc 4 samplesNb.(4)
