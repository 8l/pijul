(*
  Copyright Florent Becker and Pierre-Etienne Meunier 2015.

  This file is part of Pijul.

  Pijul is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Pijul is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Pijul.  If not, see <http://www.gnu.org/licenses/>.
*)

open Pijul

let dir_perm=0o755
let logchan=ref None
let get_logchan dir=
  match !logchan with
    Some c->c
  | None->
     begin
       let path=Filename.concat (pijuldir dir) "log" in
       let c=Unix.openfile path [Unix.O_WRONLY;Unix.O_APPEND;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
       logchan:=Some c;
       c
     end


let get_pijul_dir dir=
  let rec get_it dir l=
    if Sys.file_exists (pijuldir dir) then (
      dir,l
    ) else
      let d=Filename.dirname dir in
      let f=Filename.basename dir in
      if d=dir then "",[] else get_it d (f::l)
  in
  get_it dir []


let fc=Filename.concat
let fcc a=
  let rec fcc a r=match a with
      []->r
    | h::s->fcc s (Filename.concat r h)
  in
  match a with h::s->fcc s h | []->"/"

let split_path p=
  let rec split p l=
    if p="." || p="" then l else
      let dir=Filename.dirname p in
      if dir=p then l else
        split dir (Filename.basename p::l)
  in
  split p []

let make_absolute dir=
  if Filename.is_relative dir then
    begin
      let rec path d l=
        let d'=Filename.dirname d in
        if d=d' then d::l else
          let f=Filename.basename d in
          if f=Filename.parent_dir_name then
            path (Filename.dirname d') l
          else
            path d' (f::l)
      in
      let p=path (Filename.concat (Sys.getcwd ()) dir) [] in
      fcc p
    end
  else
    dir

let init dir=
  let dir=make_absolute dir in
  if fst (get_pijul_dir dir)="" then
    try
      let rec mkdir dir=
        if not (try Sys.is_directory dir with _->false) then (
          let dd=Filename.dirname dir in
          if dd<>dir then mkdir dd;
          Unix.mkdir dir dir_perm
        )
      in
      mkdir dir;
      let pij=pijuldir dir  in
      Unix.mkdir pij dir_perm;
      Unix.mkdir (pristinedir pij) dir_perm;
      Unix.mkdir (patchesdir pij) dir_perm;
      let o=open_out_bin (Filename.concat pij (changesfile Pijul.default_branch)) in
      close_out o;
    with
      Unix.Unix_error (Unix.EEXIST,_,x)->
      failwith ("Le dossier "^x^" existe déjà.")
  else
    failwith ("Un pijul chante déjà dans "^dir)


let from_repo repo=
  try
    let n=String.index repo ':' in
    let host=String.sub repo 0 n in
    let path=String.sub repo (n+1) (String.length repo-n-1) in
    host,path
  with
    Not_found->"",repo


let fetch_remote_changes ~pijul_dir ~host ~path ~remote_branch local_tmp socket=
  let pid=
    Unix.create_process
      "rsync"
      (if host="" then [|"rsync";path^"/"^_pijul^"/"^changesfile remote_branch;local_tmp|]
       else
         [|"rsync";
           "-e";"ssh -o ControlMaster=auto -o ControlPersist=600 -o ControlPath="^socket;
           path^"/"^_pijul^"/"^changesfile remote_branch;local_tmp|])
      Unix.stdin Unix.stdout (get_logchan pijul_dir)
  in
  pid

let waitpid comment pid=
  let _,stat=Unix.waitpid [] pid in
  if stat<>Unix.WEXITED 0 then failwith comment


let apply_recursive ~pijuldir txn alldb patchid=
  let rec apply_recursive patchid=
    if not (Pijul.branch_has_patch txn alldb (Pijul.current_branch alldb) patchid) then
      begin
        let p=open_in (Filename.concat (patchesdir pijuldir) (Pijul.to_hex patchid)) in
        let patch=try
            let patch=Pijul.input_patch p in
            close_in p;
            patch
          with e->(close_in p;raise e)
        in
        List.iter (fun dep->apply_recursive dep) patch.Pijul.dependencies;
        Pijul.apply txn alldb patch patchid;
        Pijul.write_changes ~pijuldir txn alldb
      end
  in
  apply_recursive patchid



let compare_repositories source target=
  let applicable=ref [] in
  let remo=open_in_bin target in
  let loco=open_in_bin source in
  let rem=String.make (1+Pijul.hash_size) '\000' in
  let loc=String.make (1+Pijul.hash_size) '\000' in
  let fini=ref false in
  let _=
    try
      while true do
        let c=compare rem loc in
        if c>=0 || !fini then (
          if c>0 || !fini then (applicable:=(String.sub loc 1 hash_size) :: !applicable);
          really_input loco loc 0 (1+Pijul.hash_size);
        );
        if c<=0 then
          (try really_input remo rem 0 (1+Pijul.hash_size) with End_of_file->fini:=true)
      done
    with
      End_of_file->(close_in remo;close_in loco;)
    | e->(close_in remo;close_in loco;raise e)
  in
  !applicable






let pull ?(local_branch="main") ?(remote_branch="main") (*env txn alldb*) url target ask=
  let host,path=from_repo url in
  let pijul_path=pijuldir target in
  let remote_changes=Filename.concat pijul_path (Pijul.to_hex url^"."^Pijul.to_hex remote_branch) in
  let socket=if host<>"" then Filename.concat pijul_path (Pijul.to_hex url^".ssh") else "" in
  let pid_changes=fetch_remote_changes ~pijul_dir:target ~host ~path ~remote_branch remote_changes socket in
  let pid_patches=
    Unix.create_process
      "rsync"
      (if host="" then [|"rsync";
                         "-r";url^"/"^_pijul^"/patches/";target^"/"^_pijul^"/patches/"|]
       else
         [|"rsync";"-r";
           "-e";"ssh -o ControlMaster=auto -o ControlPersist=600 -o ControlPath="^socket;
           url^"/"^_pijul^"/patches/";target^"/"^_pijul^"/patches/"
          |])
      Unix.stdin Unix.stdout (get_logchan target)
  in
  (try
      waitpid "fetch_remote_changes" pid_changes;
      waitpid "pull patches" pid_patches;
    with _->());

  let applicable=compare_repositories remote_changes (Filename.concat pijul_path (changesfile local_branch)) in
  let applicable=if ask then Interaction.filter_patches pijul_path true "pull" applicable else applicable in
  with_pijul
    pijul_path
    (fun env txn->
     let alldb=Pijul.open_repository txn in
     let records,_=Pijul.record ~working_copy:target txn alldb in
     List.iter (fun p->
                apply_recursive ~pijuldir:(pijuldir target) txn alldb p
               ) applicable;
     (* Printf.eprintf "pulled %d\n" (List.length applicable);flush stderr; *)
     if applicable<>[] then Pijul.output_repository ~working_copy:target env txn alldb records
    )

let push ?(local_branch="main") ?(remote_branch="main") source url ask=
  let host,path=from_repo url in
  let pijul_path=pijuldir source in
  let remote_changes=Filename.concat pijul_path (Pijul.to_hex url^"."^Pijul.to_hex remote_branch) in
  let socket=if host<>"" then Filename.concat pijul_path (Pijul.to_hex url^".ssh") else "" in
  let pid_changes=fetch_remote_changes ~pijul_dir:source ~host ~path ~remote_branch remote_changes socket in
  let pid_patches=
    Unix.create_process
      "rsync"
      (if host="" then [|"rsync";"-r";source^"/"^_pijul^"/patches/";url^"/"^_pijul^"/patches/";|]
       else
         [|"rsync";"-r";
           "-e";"ssh -o ControlMaster=auto -o ControlPersist=600 -o ControlPath="^socket;
           source^"/"^_pijul^"/patches/";
           url^"/"^_pijul^"/patches/";
          |])
      Unix.stdin Unix.stdout (get_logchan source)
  in
  (try
      waitpid "fetch_remote_changes" pid_changes;
      waitpid "pull patches" pid_patches;
    with _->());

  let applicable=compare_repositories (Filename.concat pijul_path (changesfile local_branch)) remote_changes in
  let applicable=if ask then Interaction.filter_patches pijul_path true "push" applicable else applicable in

  if host="" then
    begin
      let targetpijul=pijuldir path in
      Pijul.with_pijul
        targetpijul
        (fun env txn->
         let alldb=Pijul.open_repository txn in
         let records,_=Pijul.record ~working_copy:path txn alldb in
         List.iter (fun p->apply_recursive ~pijuldir:targetpijul txn alldb (String.sub p 1 Pijul.hash_size)) applicable;
         if applicable<>[] then (
           Pijul.output_repository ~working_copy:path env txn alldb records
         )
        )
    end
  else
    begin
      let pid_apply=
        Unix.create_process
          "ssh"
          [|"ssh";
           "-e";"ssh -o ControlMaster=auto -o ControlPersist=600 -o ControlPath="^socket;
            host;"cd "^path^" && pijul apply "^String.concat " " (List.map Pijul.to_hex applicable);
           |]
          Unix.stdin Unix.stdout (get_logchan source)
      in
      waitpid "remote apply" pid_apply;
    end

let remote_init dir url=
  let host,path=from_repo url in
  let pijul_path=pijuldir dir in
  if host<>"" then (
    let socket=Filename.concat pijul_path (Pijul.to_hex url^".ssh") in
    let pid=
      Unix.create_process
        "ssh"
        [|"ssh";
          "-e";"ssh -o ControlMaster=auto -o ControlPersist=600 -o ControlPath="^socket;
          host;"pijul init -path "^(Filename.quote path)
         |]
        Unix.stdin Unix.stdout (get_logchan dir)
    in
    waitpid "remote init" pid
  ) else
    init path


#ifdef DEBUG
let debug dir=
  Pijul.with_pijul
  (pijuldir dir)
  (fun _ txn->
   let alldb=Pijul.open_repository txn in
   let o=open_out "debug" in
   Pijul.debug_repository o txn alldb;
   close_out o)
#else
    let debug dir=()
#endif


  (* blanc fort: 39, gris 30, rouge 31, vert 32 *)
let filter_records action record_mode records=
  let plusfile_esc="\027[1;32m" (* foreground: 39, 30-37, 90-97. Style: 1,2,4,5,7,8 *)
  and rmfile_esc="\027[1;31m"
  (* and movefile_esc="\027[1;33m" *)
  and plus_esc="\027[32m"
  and minus_esc="\027[31m"
  and reset_esc="\027[0m" in
  let n=List.length records in
  (* Regrouper toutes les opérations de fichier relatives à une même clef, pour faire les mv/rm/add correctement. *)
  Interaction.general_filter
    records record_mode
    (fun h->
     match h with
       Pijul.Plusfile (name,_)
     | Pijul.Minusfile (name,_)->name
     | Pijul.Plus _ | Pijul.Minus _ ->"")
    (fun h->
     match h with
       Pijul.Plusfile (name,_)
     | Pijul.Minusfile (name,_)->
        let rec get_deps x l=
          try
            let i=String.rindex x '/' in
            get_deps (String.sub x 0 i) (String.sub x (i+1) (String.length x-i-1) :: l)
          with
            Not_found->l
        in
        get_deps (Filename.dirname name) []
     | Pijul.Plus _ | Pijul.Minus _ ->[]
    )
    (fun h i->
     let _=
       match h with
         Pijul.Plusfile (name,_)->
         Printf.fprintf stdout "%sadd file%s %s\n" plusfile_esc reset_esc name;
       | Pijul.Minusfile (name,_)->
          Printf.fprintf stdout "%sremove file%s %s\n" rmfile_esc reset_esc name;
       | Plus h->
          List.iter
            (fun (_,c)->
             let c=String.sub c Pijul.key_size (String.length c-Pijul.key_size) in
             Printf.fprintf stdout "%s+ %s%s" plus_esc c reset_esc;
             if if c="" then true else c.[String.length c-1]<>'\n' then Printf.fprintf stdout "\n"
            ) h
       | Minus h->
          List.iter
            (fun (_,b)->
             let c=String.sub b Pijul.key_size (String.length b-Pijul.key_size) in
             Printf.fprintf stdout "%s- %s%s" minus_esc c reset_esc;
             if if c="" then true else c.[String.length c-1]<>'\n' then Printf.fprintf stdout "\n"
            ) h
     in
     Printf.fprintf stdout "%s (%d/%d) [ynk]: " action i n;flush stdout
    )

let ask_author pijul_path=
  try
    let i=open_in (Filename.concat pijul_path "author") in
    let l=input_line i in
    close_in i;
    if l="" then raise (Sys_error "") else l
  with
    Sys_error _ ->
    begin
      let rec ask_name ()=
        Printf.fprintf stdout "What is your name (example: Marcel Dupont <marcel.dupont@example.org>)? ";
        flush stdout;
        let auth=input_line stdin in
        if auth<>"" then (
          let i=open_out (Filename.concat pijul_path "author") in
          Printf.fprintf i "%s\n" auth;
          close_out i;
          auth
        ) else
          ask_name ()
      in
      ask_name ()
    end


let ask_name ()=
  let rec ask_name ()=
    Printf.fprintf stdout "What is the patch name? ";
    flush stdout;
    let name=input_line stdin in
    if name<>"" then (
      Printf.fprintf stdout "Do you want to add a long comment [yN]?";
      flush stdout;

      let tcattr=Unix.tcgetattr Unix.stdin in
      let _=Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
                           { tcattr with Unix.c_icanon=false }
      in
      let answer=
        let str="n" in
        let _=Unix.read Unix.stdin str 0 1 in
        str
      in
      let _=Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcattr in
      if answer="n" || answer="N" then name else (
        if answer<>"\n" then Printf.fprintf stdout "\n";
        flush stdout;
        Printf.fprintf stderr "long comments not implemented\n";
        flush stderr;
        name
      )
    ) else
      ask_name ()
  in
  ask_name ()

#ifdef DEBUG
let _=Random.init 2015
#else
let _=Random.self_init ()
#endif




exception Abort

let main (command : string)  (extra : string list) (init_dir : string) (do_ask: bool) =
    match command with
    "init"->
      begin
        let path=if init_dir="" then Sys.getcwd () else init_dir in
        init path
      end
    | "record"->
       begin
	 try
           let dir, _ = get_pijul_dir init_dir in
           let pijul_path = pijuldir dir in
           Pijul.with_pijul pijul_path (fun _ txn -> Commands.record pijul_path dir txn); debug dir;
	 with
	 | Commands.Nothing_to_record -> failwith "Nothing to record"
       end
    | "unrecord"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         let pijul_path=pijuldir dir in
	 Commands.unrecord pijul_path
       end
    | "rollback"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         let pijul_path=pijuldir dir in
         Commands.rollback pijul_path
       end

    | "revert"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         let _=
           try
             Pijul.with_pijul
               (pijuldir dir)
               (fun _ txn->
                let repo=Pijul.open_repository txn in
                let records,_=Pijul.record ~working_copy:dir txn repo in
                let records=
                  (filter_records "Do you want to revert this change?" false records)
                in
                let patch={Pijul.empty_patch with records} in
                let patchid=String.make Pijul.hash_size '\255' in
                Pijul.unsafe_apply txn repo patch patchid;
                Pijul.unsafe_output_repository ~working_copy:dir true txn repo;
                raise Abort
               )
           with
             Abort -> ()
           | e -> raise e
         in
         debug dir;
       end
    | "add"
    | "remove"->
       begin
         match extra with
           []->failwith "Pas de fichier à ajouter"
         | _->
            let dir,path=get_pijul_dir (Sys.getcwd()) in
            Pijul.with_pijul
              (pijuldir dir)
              (fun _ txn->
               let alldb=Pijul.open_repository txn in
               List.iter
                 (fun file->
                  if command="add" then
                    if Sys.file_exists file then
                      let file=Str.split (Str.regexp "/") file in
                      Pijul.addfile txn alldb ((path@file))
                    else
                      failwith ("Fichier inexistant: "^file)
                  else
                    let file=Str.split (Str.regexp "/") file in
                    Pijul.delfile txn alldb ((path@file))
                 ) extra;
              )
       end
    | "mv" ->
       begin
         (* Un peu chiant : il y a trois cas différents de mv. *)
         let moves=
           match extra with
             _::_::h::s-> (* Au moins trois fichiers : tester si le dernier est un répertoire. *)
             let rec last l=match l with [h]->h | _::s->last s | []->assert false in
             let rec files l=match l with [_]->[] | h::s->h::files s | []->assert false in
             let b=last (h::s) in
             if try Sys.is_directory b with _->false then
               List.map (fun a->a,fc b (Filename.basename a)) (files extra)
             else
               failwith "move 1"
           | a::b::_->
              if try Sys.is_directory b with _->false then
                [a,fc b (Filename.basename a)]
              else
                [a,b]
           | _->failwith "move 2"
         in
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         Pijul.with_pijul
           (pijuldir dir)
           (fun _ txn->
            let alldb=Pijul.open_repository txn in
            List.iter (fun (a,b)->
                       let f=Str.split (Str.regexp "/") a
                       and f'=Str.split (Str.regexp "/") b in
                       Pijul.movefile txn alldb f f';
                       Sys.rename (fc dir a) (fc dir b);
                      ) moves
           )
       end
    | "get"->
       begin
         match extra with
           url::target->
           begin
             let target=match target with
                 []->
                 (try let i=String.rindex url '/' in String.sub url (i+1) (String.length url-i-1)
                  with Not_found->
                       try let i=String.rindex url ':' in String.sub url (i+1) (String.length url-i-1)
                       with Not_found->failwith "get: mauvaise url")
               | h::_->h
             in
             Unix.mkdir target dir_perm;
             init target;
             (* Printf.fprintf stderr "init done\n";flush stderr; *)
             pull url target false;
             debug target
           end
         | []->failwith "get: pas de source"
       end
    | "pull"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         match extra with
           [url]->pull url dir do_ask
         | []->failwith "pull: pas assez de dépôts"
         | _::_->failwith "pull: trop de dépôts"
       end
    | "put"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         match extra with
           [url]->(
           remote_init dir url;
           push dir url false
         )
         | []->failwith "put: pas assez de dépôts"
         | _::_->failwith "put: trop de dépôts"
       end
    | "push"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         match extra with
           [url]->
           push dir url do_ask
         | []->failwith "push: pas assez de dépôts"
         | _::_->failwith "push: trop de dépôts"
       end
    | "changes" | "log"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         let pijul_path=(pijuldir dir) in
         let patches=
           Pijul.with_pijul
             pijul_path
             (fun _ txn->
              let repo=Pijul.open_repository txn in
              branch_patches txn repo (Pijul.current_branch repo)
             )
         in
         List.iter (fun h->
                    let o=open_in_bin (Filename.concat (patchesdir pijul_path) (to_hex h)) in
                    let p=Pijul.input_patch o in
                    close_in o;
                    Printf.fprintf stdout "%s %s %s\n  * %s\n" p.time p.author (to_hex h) p.name
                   ) patches;
       end
    | "apply"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         let pijul_path=pijuldir dir in
         let patches_path=patchesdir pijul_path in
         Pijul.with_pijul
           pijul_path
           (fun env txn->
            let alldb=Pijul.open_repository txn in
            let records,_=Pijul.record ~working_copy:dir txn alldb in
            List.iter (fun patchid->
                       let o=open_in (fc patches_path patchid) in
                       let p=Pijul.input_patch o in
                       close_in o;
                       Pijul.apply txn alldb p (Pijul.from_hex patchid)
                      ) extra;
            Pijul.write_changes ~pijuldir:pijul_path txn alldb;
            Pijul.output_repository ~working_copy:dir env txn alldb records
           );
         debug dir
       end
    | "ls"->
       begin
         let dir,_=get_pijul_dir (Sys.getcwd()) in
         let pijul_path=pijuldir dir in
         Pijul.with_pijul
           pijul_path
           (fun _ txn->
            let rep=Pijul.open_repository txn in
            let _=
              tree_iter
                txn rep ""
                (fun path base->
                 let p=Filename.concat path base in
                 Printf.printf "%s\n" p;
                 p
                )
            in
            ()
           );
       end
    | "debug"->
       let dir,_=get_pijul_dir (Sys.getcwd()) in
       debug dir
    | cmd->
       begin
         failwith ("commande inconnue: "^cmd)
       end


let _=
  Sys.catch_break true;
  let spec=ref [] in
  let command=ref "" in
  let extra=ref [] in
  let init_dir=ref "" in
  let do_ask=ref true in
  Arg.parse_dynamic
    spec
    (fun anon->
     if !command="" then
       command:=anon
     else (
       extra:= anon :: !extra
     );
     match !command with
       "init"->spec:=[("-path",Arg.String (fun x->init_dir:=x),"Chemin à initialiser");
                      ("-help",Arg.Unit (fun ()->raise (Arg.Help "")), "Aide")
                     ]
     | "pull" | "push" -> spec:=[("-a",Arg.Unit (fun ()->do_ask:=false), "Ne pas demander")]
     | _->()
    )
    "usage";
  let extra=List.rev !extra in
  try
    main !command extra !init_dir !do_ask;
  with
    e->
      begin
        let _=match !logchan with None->() | Some c->Unix.close c in
        match e with
          _->raise e
      end
