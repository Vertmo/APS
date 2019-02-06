let samplesAPS0loc = "../../samples/aps0/" and samplesAPS0nb = 21

let generateFileNames loc nb =
  List.init nb (fun i -> let si = (string_of_int i) in
                 let si = (String.make (3 - (String.length si)) '0')^si in
                 Printf.sprintf "%sprog%s.aps" loc si)

let testParse f =
  if Sys.command (Printf.sprintf "./src/aps.exe -parse %s > /dev/null" f) <> 0
  then failwith (Printf.sprintf "Parse failed for file %s" f)

let testType f =
  if Sys.command (Printf.sprintf "./src/aps.exe -type %s > /dev/null " f) <> 0
  then failwith (Printf.sprintf "Typing failed for file %s" f)

let generateAndTest loc nb =
  let files = generateFileNames loc nb in
  List.iter testParse files;
  Printf.printf "Tested parse for %s successfully !\n" loc;
  List.iter testType files;
  Printf.printf "Tested typing for %s successfully !\n" loc

let _ =
  generateAndTest samplesAPS0loc samplesAPS0nb
