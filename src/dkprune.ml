open Basic

exception NoDirectory
exception EntryNotHandled of Entry.entry
exception BadFormat
exception NoPruneFile

let output_directory : string option ref = ref None

type constraints = Dep.NameSet.t

let _ =
  Dep.ignore := true;
  Dep.compute_ideps := true

let rec handle_file : string -> unit =
  let computed = ref Dep.MDepSet.empty in
  fun file ->
    let md = Env.Default.init file in
    if not @@ Dep.MDepSet.mem (md,file) !computed then
      begin
        computed := Dep.MDepSet.add (md,file) !computed;
        let input = open_in file in
        Dep.handle md (fun f -> Parser.Parse_channel.handle md f input);
        close_in input;
        let md_deps = Hashtbl.find Dep.deps md in
        Dep.MDepSet.iter (fun (md,_) -> try handle_file (Dep.get_file md) with _ -> ()) md_deps.deps
      end

let handle_name : Basic.name -> unit = fun name ->
  let md = Basic.md name in
  let file = Dep.get_file md in
  handle_file file

let handle_constraints names =
  Dep.NameSet.iter handle_name names

let output_file : Dep.path -> Dep.path = fun file ->
  let basename = Filename.basename file in
  match !output_directory with
  | None -> raise @@ NoDirectory
  | Some dir -> Filename.concat dir basename

let get_files : unit -> (mident * Dep.path * Dep.path) list = fun () ->
  Hashtbl.fold (fun md _ l ->
      try
        let f = Dep.get_file md in
        (md, f, output_file f)::l
    with _ -> l) Dep.deps []

let name_of_entry md = function
  | Entry.Decl(_,id,_,_) ->
    mk_name md id
  | Entry.Def(_,id,_,_,_) ->
    mk_name md id
  | Entry.Rules(_,r::_) ->
    let open Rule in
    let r' = to_rule_infos r in
    r'.cst
  | _ as e -> raise @@ EntryNotHandled e

let mk_entry deps md fmt e =
  let name = name_of_entry md e in
  if Dep.NameSet.mem name deps then
    Format.fprintf fmt "%a" Pp.print_entry e

let mk_file deps (md,in_file,out_file) =
  let input = open_in in_file in
  let output = open_out out_file in
  let fmt = Format.formatter_of_out_channel output in
  Parser.Parse_channel.handle md (fun e -> Format.fprintf fmt "%a" (mk_entry deps md) e) input;
  close_out output;
  close_in input

let print_dependencies cstr =
  let open Dep in
  NameSet.iter Dep.transitive_closure cstr;
  let down = NameSet.fold
      (fun name dependencies ->
         NameSet.union (get_data name).down dependencies) cstr cstr in
  let files = get_files () in
  List.iter (mk_file down) files

let mk_cstr =
  let open Rule in
  function
  | Entry.Rules(_,[r]) ->
    begin
      match r.pat with
      | Pattern(_,name,_) -> name
      | _ -> raise BadFormat
    end
  | _ -> raise BadFormat

let parse_constraints : string -> Dep.NameSet.t = fun file ->
  let input = open_in file in
  let md = mk_mident file in
  let cstr = List.map mk_cstr (Parser.Parse_channel.parse md input) in
  close_in input;
  Dep.NameSet.of_list cstr

let _ =
  (* Parsing of command line arguments. *)
  let sorted  = ref false   in
  let args = Arg.align
    [ ( "-d"
      , Arg.String Env.set_debug_mode
      , "FLAGS enables debugging for all given flags:
      q : (quiet)    disables all warnings
      n : (notice)   notifies about which symbol or rule is currently treated
      o : (module)   notifies about loading of an external module (associated
                     to the command #REQUIRE)
      c : (confluence) notifies about information provided to the confluence
                     checker (when option -cc used)
      u : (rule)     provides information about type checking of rules
      t : (typing)   provides information about type-checking of terms
      r : (reduce)   provides information about reduction performed in terms
      m : (matching) provides information about pattern matching" )
    ; ( "-v"
      , Arg.Unit (fun () -> Env.set_debug_mode "montru")
      , " Verbose mode (equivalent to -d 'montru')" )
    ; ( "-q"
      , Arg.Unit (fun () -> Env.set_debug_mode "q")
      , " Quiet mode (equivalent to -d 'q')" )
    ; ( "-s"
      , Arg.Set sorted
      , " Sort the source files according to their dependencies" )
    ; ( "--ignore"
      , Arg.Set Dep.ignore
      , " If some dependencies are not found, ignore them" )
    ; ( "-I"
      , Arg.String add_path
      , "DIR Add the directory DIR to the load path" )
    ; ( "-o"
      , Arg.String (fun s -> output_directory := Some s)
      , " Set the output directory" ) ]
  in
  let usage = Format.sprintf "Usage: %s [OPTION]... [FILE]...
Compute the dependencies of the given Dedukti FILE(s).
For more information see https://github.com/Deducteam/Dedukti.
Available options:" Sys.argv.(0) in
  let files =
    let files = ref [] in
    Arg.parse args (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  try
    let cstr =
      List.fold_left
        (fun set file -> Dep.NameSet.union (parse_constraints file) set) Dep.NameSet.empty files
    in
    handle_constraints cstr;
    print_dependencies cstr
  with
  | Dep.Dep_error dep -> Errors.fail_env_error dloc (Env.EnvErrorDep dep)
  | Sys_error err     -> Errors.fail_sys_error err
