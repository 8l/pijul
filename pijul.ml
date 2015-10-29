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

type repository={ current_branch:string; dbi_nodes:Mdb.dbi; dbi_patches:Mdb.dbi; dbi_branches: Mdb.dbi; dbi_tree: Mdb.dbi; dbi_revtree:Mdb.dbi; dbi_inodes: Mdb.dbi; dbi_revinodes:Mdb.dbi }
let current_branch alldb=alldb.current_branch
let default_branch="main"
let open_repository txn=
  let dbi_nodes=Mdb.dbi_open txn "nodes" (Mdb._CREATE lor Mdb._DUPSORT) in
  let dbi_patches=Mdb.dbi_open txn "patches" (Mdb._CREATE lor Mdb._DUPSORT) in
  let dbi_branches=Mdb.dbi_open txn "branches" (Mdb._CREATE lor Mdb._DUPSORT) in
  let dbi_tree=Mdb.dbi_open txn "tree" (Mdb._CREATE lor Mdb._DUPSORT) in
  let dbi_revtree=Mdb.dbi_open txn "revtree" (Mdb._CREATE) in
  let dbi_inodes=Mdb.dbi_open txn "inodes" (Mdb._CREATE) in
  let dbi_revinodes=Mdb.dbi_open txn "revinodes" (Mdb._CREATE) in
  let current_branch=
    try Mdb.get txn dbi_branches "\000" with Mdb.Notfound->default_branch
  in
  { current_branch;dbi_nodes;dbi_patches;dbi_branches;dbi_tree;dbi_revtree;dbi_inodes;dbi_revinodes }

let fc=Filename.concat
         (*
let split_path path=
  try
    let i=String.rindex path '/' in
    String.sub path 0 i, String.sub path (i+1) (String.length path-i-1)
  with
    Not_found->"",path
          *)
let to_hex digest=
  let str=String.create (String.length digest*2) in
  let hexletter x=
    if x<10 then (char_of_int (int_of_char '0'+x)) else char_of_int (int_of_char 'a' + x-10)
  in
  for i=0 to String.length digest-1 do
    str.[i*2]<-hexletter (int_of_char digest.[i] lsr 4);
    str.[i*2+1]<-hexletter (int_of_char digest.[i] land 0xf);
  done;
  str

let from_hex hex_patch_id=
  let id=String.create (String.length hex_patch_id / 2) in
  let from_hex c=
    let a=int_of_char c-int_of_char '0' in
    if a>=0 && a<=9 then a else (int_of_char c-int_of_char 'a'+10)
  in
  for i=0 to String.length id-1 do
    id.[i]<-char_of_int ((from_hex hex_patch_id.[2*i] lsl 4) lor (from_hex hex_patch_id.[2*i+1]))
  done;
  id

let _pijul=".pijul"
let pijuldir x=Filename.concat x _pijul
let patchesdir x=Filename.concat x "patches"
let pristinedir x=Filename.concat x "pristine"
let changesfile branch=("changes."^(to_hex branch))


let with_env path f=
  let env=Mdb.env_create () in
  try
    Mdb.env_set_maxdbs env 7;
    let _=Mdb.reader_check env in
    Mdb.env_open env path 0 0o750;
    let x=f env in
    Mdb.env_close env;
    x
  with e->(Mdb.env_close env;raise e)

let with_txn env parent f=
  let t=Mdb.txn_begin env parent 0 in
  try let x=f t in Mdb.txn_commit t; x
  with e->(Mdb.txn_abort t;raise e)

let with_pijul pijul_path f=
  with_env (pristinedir pijul_path) (fun env->with_txn env None (fun txn->f env txn))


let with_cursor txn dbi f=
  let curs=Mdb.cursor_open txn dbi in
  try let x=f curs in Mdb.cursor_close curs; x with e->(Mdb.cursor_close curs;raise e)


type record=
    Plusfile of string*((string*string) list)
  | Minusfile of string*((string*string) list)
  | Plus of (string*string) list
  | Minus of (string*string) list



type patch=
    { records:record list;
      dependencies:string list;
      obsoletes:string list;
      name:string;
      author:string;
      time:string;
    }
let empty_patch={records=[];dependencies=[];obsoletes=[];name="";author="";time=""}
let hash_size=20
let line_size=4
let key_size=hash_size+line_size
let root_key=String.make key_size '\000'

(*
let time_str ()=
  let t=int_of_float (Unix.time ()) in
  let s=String.create 4 in
  s.[0]<-char_of_int ((t lsr 24) land 0xff);
  s.[1]<-char_of_int ((t lsr 16) land 0xff);
  s.[2]<-char_of_int ((t lsr 8) land 0xff);
  s.[3]<-char_of_int (t land 0xff);
  s
 *)
let sha1 file=
  let buf=String.create 64 in
  let sha1_=Cryptokit.Hash.sha1 () in
  let o=open_in_bin file in
  try
    let rec inp ()=
      let len=input o buf 0 (String.length buf) in
      if len>0 then (
        sha1_#add_substring buf 0 len;
        inp ()
      )
    in
    inp ();
    close_in o;
    (sha1_#result)
  with
    e->(close_in o;raise e)

let skip_values=
  let buf=String.create (Marshal.header_size) in
  let rec skip_values n o=
    if n>0 then
      begin
        really_input o buf 0 Marshal.header_size;
        seek_in o (pos_in o + Marshal.data_size buf 0);
        skip_values (n-1) o
      end
  in
  skip_values

let output_patch o p=
  output_value o p.time;
  output_value o p.dependencies;
  output_value o p.obsoletes;
  output_value o p.name;
  output_value o p.author;
  output_value o p.records

let input_patch o=
  let time=input_value o in
  let dependencies=input_value o in
  let obsoletes=input_value o in
  let name=input_value o in
  let author=input_value o in
  let records=input_value o in
  { records;dependencies;obsoletes;name;author;time }

let input_dependencies o=
  skip_values 1 o;
  input_value o

let input_time o=
  input_value o


let rec streq a i b j l=
  if l=0 then 0 else
    let c=compare a.[i] b.[j] in
    if c=0 then
      streq a (i+1) b (j+1) (l-1)
    else
      c

let node_of_val x=String.sub x (1+hash_size) key_size

(** Iterates function f over all (k,v) in the database, such that k=key and char is a prefix of v.
Raises Invalid_argument if one of these values of v is strictly shorter than char. *)
let neighbors_iter txn db key f char=
  (* In this module, this function is used to (1) iterate over one specific type of edges and (2) iterate over all edges created by a specific patch. *)
  with_cursor
    txn db.dbi_nodes
    (fun curs->
     try
       let rec iterate k b=
         let c=streq b 0 char 0 (String.length char) in
         if c=0 then f b else
           if c>0 then raise Mdb.Notfound;
         let k,b=Mdb.cursor_get curs k b Mdb._NEXT_DUP in
         iterate k b
       in
       let k,_=Mdb.cursor_get curs key "" Mdb._SET_RANGE in
       if streq k 0 key 0 key_size <> 0 then raise Mdb.Notfound
       else (
         let str=String.make (1+key_size+hash_size) '\000' in
         String.blit char 0 str 0 (String.length char);
         let _,b=Mdb.cursor_get curs k str Mdb._GET_BOTH_RANGE in
         iterate k b
       )
     with
       Mdb.Notfound->()
    )



module S=Set.Make(String)
module M=Map.Make(String)
let str c=String.make 1 (char_of_int c)
let c c=char_of_int c

let is_new c=int_of_char c land 16 <> 0
let is_deleted c=int_of_char c land 8 <> 0
let is_pseudo c=int_of_char c land 1 <> 0
(*
(* These two are not used elsewhere, but are left here for
documentation purposes (and also to make sure flags do not
overlap). *)
let is_folder c=int_of_char c land 2 <> 0
let is_children c=int_of_char c land 4 <> 0
 *)
let new_flag=16
let deleted_flag=8
let parent_flag=4
let folder_flag=2
(* let folder_edge_flag=folder_flag lor edge_flag *)
let pseudo_flag=1

let change_direction c=char_of_int (int_of_char c lxor 4)
let change_deleted c=char_of_int (int_of_char c lxor 8)
(* let change_pseudo c=char_of_int @@ int_of_char c lxor 1 *)


(* let children_iter txn db key f=neighbors_iter txn db key f (str edge_flag) *)
(* let parents_iter txn db key f=neighbors_iter txn db key f (str parent_flag) *)
let folder_children_iter txn db key f=neighbors_iter txn db key f (str (folder_flag))

let pseudo_children_iter txn db key f=
  neighbors_iter txn db key f (str 0);
  neighbors_iter txn db key f (str pseudo_flag)


let folder_parents_iter txn db key f=neighbors_iter txn db key f (str (parent_flag lor folder_flag))

(** Iterate on all non-pseudo parents. *)
let real_parents_iter txn db key f=
  neighbors_iter txn db key f (str parent_flag);
  neighbors_iter txn db key f (str (parent_flag lor folder_flag));
  neighbors_iter txn db key f (str (parent_flag lor deleted_flag));
  neighbors_iter txn db key f (str (parent_flag lor folder_flag lor deleted_flag))


exception Found



let create_num n=
  let rec int_blit str off i n=
    if i<line_size then (str.[off+i]<-char_of_int (n land 0xff); int_blit str off (i+1) (n lsr 8))
  in
  let c=String.create line_size in
  int_blit c 0 0 n;
  c


let inode_size=10
let root_inode=String.make inode_size '\000'

(** create_new_inode must be called with a cursor on the dbi_revtree base. *)
let create_new_inode curs=
  let inode=String.create inode_size in
  try
    while true do
      for i=0 to inode_size-1 do
        inode.[i]<-char_of_int (Random.int 0x100)
      done;
      let k,_=Mdb.cursor_get curs inode "" Mdb._SET_RANGE in
      if streq k 0 inode 0 inode_size <> 0 then raise Mdb.Notfound
    done;
    ""
  with
    Mdb.Notfound->inode

let rec apply_hunk curs extra patch_id hunk=
  match hunk with
    Plus lines->
    begin
      (* Printf.eprintf "apply plus\n";flush stderr; *)
      List.iter
        (fun (a,b)->
         (* Printf.eprintf "+%S %S\n" (to_hex a) (to_hex b);flush stderr; *)
         let akey=if String.length a<key_size then patch_id^a else a in
         if (int_of_char b.[0]) land new_flag <> 0 then (
           let bkey=patch_id^(String.sub b 1 line_size)
           and bcont=String.sub b (1+line_size) (String.length b-1-line_size)
           and flag=String.sub b 0 1 in
           flag.[0]<-change_direction flag.[0];
           flag.[0]<-char_of_int (int_of_char flag.[0] lxor new_flag);
           Mdb.cursor_put curs (bkey^bcont) (flag^patch_id^akey) 0;
           flag.[0]<-change_direction flag.[0];
           extra:=(akey,flag^patch_id^bkey):: !extra;
         ) else (
           let flag=String.sub b 0 1 in
           let bk=
             let bk=fst (Mdb.cursor_get curs (String.sub b 1 key_size) "" Mdb._SET_RANGE) in
             (* if streq bk 0 b 1 key_size=0 then bk else raise Mdb.Notfound *)
             bk
           in
           flag.[0]<-change_direction flag.[0];
           Mdb.cursor_put curs bk (flag^patch_id^akey) 0;
           flag.[0]<-change_direction flag.[0];
           extra:=(akey,flag^patch_id^(String.sub b 1 key_size)):: !extra;
         )
        ) lines
    end
  | Minus edges->
     begin
       (* Printf.eprintf "apply minus\n";flush stderr; *)
       List.iter
         (fun (u,v)->
          try
            (* Printf.eprintf "-%S %S\n" (to_hex u) (to_hex v);flush stderr; *)
            let ukey,_=Mdb.cursor_get curs u "" Mdb._SET_RANGE in
            if streq ukey 0 u 0 key_size <> 0 then raise Mdb.Notfound;
            let _=Mdb.cursor_get curs ukey v Mdb._GET_BOTH in
            Mdb.cursor_del curs 0;
            let vv=String.copy v in
            vv.[0]<-change_deleted vv.[0];
            String.blit patch_id 0 vv 1 hash_size;
            Mdb.cursor_put curs ukey vv 0;

            let u'=node_of_val v
            and v'=String.sub v 0 (1+hash_size) ^ u in
            v'.[0]<-change_direction v'.[0];
            let ukey',_=Mdb.cursor_get curs u' "" Mdb._SET_RANGE in
            let _=Mdb.cursor_get curs ukey' v' Mdb._GET_BOTH in
            Mdb.cursor_del curs 0;
            String.blit patch_id 0 v' 1 hash_size;
            v'.[0]<-change_deleted v'.[0];
            Mdb.cursor_put curs ukey' v' 0;
          with
            Mdb.Notfound->(
          (* If we reached this point, and have called the safe variants of the functions (which check dependencies), it means the edges to be inverted have already been inverted by another patch. *)
          )
         ) edges
     end
  | Plusfile (_,h)->apply_hunk curs extra patch_id (Plus h)
  | Minusfile (_,h)->apply_hunk curs extra patch_id (Minus h)


let unsafe_apply txn alldb patch patch_id=
  (* A node is "alive" when no deleted edge points to it, and dead if no non-deleted edge points to it.
It is a "zombie" else.
This function has three duties:
1. apply the hunks
2. if the patch adds a line after a dead line, reconnect the graph (because other functions are not supposed to follow dead edges). This is a potentially lengthy operation, depending on the size of history. However, it is still linear in the number of lines in conflict.
   This is done by connect_plus.
3. if the patch deletes a line b, we need to reconnect the alive parents of b (if any) to the alive descendants of b.
   This is done by connect_minus.
*)

  (* We start by applying the hunks. *)
  let extra=ref [] in
  with_cursor
    txn alldb.dbi_nodes
    (fun curs->
     List.iter (apply_hunk curs extra patch_id) patch.records;
     List.iter (fun (a,b)->
                let akey=try
                    let k=fst (Mdb.cursor_get curs a "" Mdb._SET_RANGE) in
                    if streq k 0 a 0 key_size=0 then k else raise Mdb.Notfound
                  with Mdb.Notfound->a
                in
                Mdb.cursor_put curs akey b 0;
               ) !extra;
     extra:=[];

     let connect_plus a b=
       (* connect_ancestors a b adds an edge between the alive ancestors of a and b (used if a is not alive). *)
       if String.length a>=key_size then
         begin
           let b=if is_new b.[0] then patch_id^String.sub b 1 line_size else String.sub b 1 key_size in
           let stop=ref S.empty in
           let rec connect_ancestors first a=
             if not (S.mem a !stop) then
               begin
                 neighbors_iter
                   txn alldb a (fun edge->connect_ancestors false (node_of_val edge)) (str (parent_flag lor deleted_flag));
                 if not first then
                   (* is a alive or zombie ? if so, raise Found and add an edge from a to b. *)
                   (* This is slightly different from connect_descendants,
            since here the only proof of life might be a folder
            edge. *)
                   try
                     neighbors_iter txn alldb a (fun _->raise Found) (str parent_flag);
                     neighbors_iter txn alldb a (fun _->raise Found) (str (parent_flag lor pseudo_flag));
                     neighbors_iter txn alldb a (fun _->raise Found) (str (parent_flag lor folder_flag));
                     neighbors_iter txn alldb a (fun _->raise Found) (str (parent_flag lor folder_flag lor pseudo_flag));
                   with
                     Found->extra:=(a,b):: !extra
               end
           in
           connect_ancestors true a
         end
     in
     let connect_minus a b=
       (* If a is alive, and this patch deletes b, then connect a to the alive descendants of b. *)
       try
         neighbors_iter txn alldb a (fun _->raise Found) (str parent_flag);
         neighbors_iter txn alldb a (fun _->raise Found) (str (parent_flag lor pseudo_flag));
         neighbors_iter txn alldb a (fun _->raise Found) (str (parent_flag lor folder_flag));
         neighbors_iter txn alldb a (fun _->raise Found) (str (parent_flag lor folder_flag lor pseudo_flag));
       with
         Found->
         begin
           let stop=ref S.empty in
           let rec connect_descendants b=
             if not (S.mem b !stop) then
               begin
                 stop:=S.add b !stop;
                 neighbors_iter txn alldb b (fun edge->extra:=(a,node_of_val edge):: !extra) (str 0);
                 neighbors_iter txn alldb b (fun edge->connect_descendants (node_of_val edge)) (str deleted_flag);
               end
           in
           connect_descendants b
         end
     in
     (* apply connect_plus and connect_minus to the patch. The only effect of this is to fill variable "extra".*)
     List.iter
       (fun act->
        match act with
          Plusfile (_,h)
        | Plus h->List.iter (fun (a,b)->connect_plus a b) h
        | Minusfile (_,h)
        | Minus h->List.iter (fun (a,b)->connect_minus a (node_of_val b)) h
       ) patch.records;

     (* Then, add all edges from !extra. *)
     List.iter (fun (a,b)->
                let akey=fst (Mdb.cursor_get curs a "" Mdb._SET_RANGE) in
                let flag=str pseudo_flag in
                Mdb.cursor_put curs akey (flag^patch_id^b) 0;
                flag.[0]<-change_direction flag.[0];
                let bkey=fst (Mdb.cursor_get curs b "" Mdb._SET_RANGE) in
                Mdb.cursor_put curs bkey (flag^patch_id^a) 0
               ) !extra
    )


let dependencies actions=
  let hunk_deps deps hunk=match hunk with
      Plusfile (_,l)
    | Plus l->
      List.fold_left
        (fun deps (a,b)->
         (* Printf.eprintf "plus deps: %S %S\n" (to_hex a) (to_hex b);flush stderr; *)
         let deps=if String.length a>=key_size then
                    if streq a 0 root_key 0 hash_size=0 then deps else
                      S.add (String.sub a 0 hash_size) deps
                  else deps
         in
         let deps=if not (is_new b.[0]) then
                    if streq b 1 root_key 0 hash_size=0 then deps else
                      S.add (String.sub b 1 hash_size) deps
                  else deps
         in
         deps) deps l
    | Minusfile (_,arr)
    | Minus arr->
         (List.fold_left
            (fun deps (a,b)->
             (* Printf.eprintf "minus deps: %S %S\n" (to_hex a) (to_hex b);flush stderr; *)
             let deps=if streq a 0 root_key 0 hash_size=0 then deps else S.add (String.sub a 0 hash_size) deps in
             let deps=if streq b 1 root_key 0 hash_size=0 then deps else S.add (String.sub b 1 hash_size) deps in
             deps
            ) deps arr)
  in
  S.elements (List.fold_left hunk_deps S.empty actions)


let branch_has_patch txn alldb branch patch=
  with_cursor
    txn alldb.dbi_branches
    (fun curs->
     try
       let _,_=Mdb.cursor_get curs branch ("\001"^patch) Mdb._GET_BOTH in
       true
     with
       Mdb.Notfound->false
    )

let branch_patches txn repo current_branch=
  let all_patches=ref [] in
  with_cursor
    txn repo.dbi_branches
    (fun cursor->
     let rec get_all_patches k a=
       all_patches:=(String.sub a 1 hash_size) :: !all_patches;
       let k',a'=Mdb.cursor_get cursor current_branch "" Mdb._NEXT in
       if k'=k then get_all_patches k' a'
     in
     try
       let k,a=Mdb.cursor_get cursor current_branch "" Mdb._SET in
       get_all_patches k a;
     with Mdb.Notfound->()
    );
  !all_patches

let delete_edges txn alldb node edges=
  if node="" then edges else
    begin
      let edges=ref edges in
      real_parents_iter
        txn alldb node
        (fun parent->
         if not (is_pseudo parent.[0]) then (
           let par=node_of_val parent in
           parent.[0]<-change_direction parent.[0];
           String.blit node 0 parent (1+hash_size) key_size;
           edges:=(par,parent):: !edges);
        );
      !edges
    end

let rollback txn alldb records patch_id=
  let rollback_plus lines=
    List.fold_left
      (fun l (_,b)->
       let bkey=if is_new b.[0] then
                  patch_id ^ String.sub b 1 line_size
                else (String.sub b 1 key_size)
       in
       delete_edges txn alldb bkey l
      ) [] lines
  in
  let rollback_minus lines=
    List.fold_left
      (fun l (_,b)->
       let bb=node_of_val b in
       List.fold_left
         (fun l (u,v)->
          (u,v)::l
         ) l (delete_edges txn alldb bb l)
      ) [] lines
  in
  let hunk_rollback l hunk=match hunk with
      Plusfile (h,lines)->Minusfile (h,rollback_plus lines)::l
    | Plus lines->Minus (rollback_plus lines)::l
    | Minusfile (h,lines)->Minusfile (h,rollback_minus lines)::l
    | Minus lines->Minus (rollback_minus lines)::l
  in
  List.fold_left hunk_rollback [] records


(* unsafe = ne vérifie pas les dépendances. *)
let unsafe_unrecord txn alldb patch patch_id=
  let kill_edge curs akey bkey flag=
    (* Printf.eprintf "kill_edge %S %S\n" (to_hex akey) (to_hex bkey);flush stderr; *)
    let _=try
        let akey,_=Mdb.cursor_get curs akey "" Mdb._SET_RANGE in
        (* Printf.eprintf "%S %S\n" (to_hex akey) (to_hex@@flag^patch_id^bkey);flush stderr; *)
        let _=Mdb.cursor_get curs akey (flag^patch_id^bkey) Mdb._GET_BOTH_RANGE in
        Mdb.cursor_del curs 0;
      with Mdb.Notfound->((* Printf.fprintf stderr "not found %d\n" __LINE__;flush stderr; *))
    in
    flag.[0]<-change_direction flag.[0];
    let _=
      try
        let bkey,_=Mdb.cursor_get curs bkey "" Mdb._SET_RANGE in
        let _=Mdb.cursor_get curs bkey (flag^patch_id^akey) Mdb._GET_BOTH_RANGE in
        Mdb.cursor_del curs 0;
      with
        Mdb.Notfound->((* Printf.fprintf stderr "not found %d\n" __LINE__;flush stderr; *))
    in
    (* Printf.eprintf "/kill_edge\n";flush stderr; *)
    flag.[0]<-change_direction flag.[0];
  in
  let kill=ref [] in
  let unrecord_hunk curs h=
    match h with
      Plusfile (_,l)
    | Plus l->
      List.iter
        (fun (a,b)->
         let akey=if String.length a<key_size then patch_id^a else a in
         let flag=String.sub b 0 1 in
         let bkey=if is_new b.[0] then (
                    flag.[0]<-char_of_int (int_of_char b.[0] lxor new_flag);
                    patch_id^(String.sub b 1 line_size)
                  ) else String.sub b 1 key_size
         in
         neighbors_iter
           txn alldb akey
           (fun parent->kill:=(akey,node_of_val parent, String.sub parent 0 1) :: !kill)
           (str (parent_flag lor pseudo_flag) ^ patch_id);

         kill_edge curs akey bkey flag;
        ) l
    | Minusfile (_,edges)
    | Minus edges->
       List.iter
         (fun (u,v)->
          let u=String.sub u 0 key_size in
          let ukey,_=Mdb.cursor_get curs u "" Mdb._SET_RANGE in
          Mdb.cursor_put curs ukey v 0;
          let u'=node_of_val v
          and v'=(String.sub v 0 (1+hash_size)) ^ u in
          v'.[0]<-change_direction v'.[0];
          let ukey',_=Mdb.cursor_get curs u' "" Mdb._SET_RANGE in
          Mdb.cursor_put curs ukey' v' 0;
          neighbors_iter
            txn alldb u
            (fun children->kill:=(u,node_of_val children,String.sub children 0 1) :: !kill)
            (str (pseudo_flag) ^ patch_id);
         ) edges
  in
  with_cursor
    txn alldb.dbi_nodes
    (fun curs->
     List.iter (fun act->unrecord_hunk curs act) patch.records;
     List.iter (fun (a,b,c)->kill_edge curs a b c) !kill
    );
  with_cursor
    txn alldb.dbi_branches
    (fun curs->
     try let _=Mdb.cursor_get curs alldb.current_branch ("\001"^patch_id) Mdb._GET_BOTH in
         Mdb.cursor_del curs 0
     with Mdb.Notfound->failwith "unrecord: patch is not applied")


let unrecord_sync txn alldb patch_id=
  with_cursor
    txn alldb.dbi_revinodes
    (fun curs_rev->
     with_cursor
       txn alldb.dbi_inodes
       (fun curs->
        let l=ref [] in
        let rec iterate a b=
          if streq a 0 patch_id 0 hash_size = 0 then (
            l:=(a,b):: !l;
            let a',b'=Mdb.cursor_get curs_rev a b Mdb._NEXT in
            iterate a' b'
          )
        in
        let _=
          try
            let a,b=Mdb.cursor_get curs_rev patch_id "" Mdb._SET_RANGE in
            iterate a b
          with Mdb.Notfound->()
        in
        List.iter
          (fun (a,b)->
           let _=Mdb.cursor_get curs_rev a "" Mdb._SET in
           Mdb.cursor_del curs_rev 0;
           let _=Mdb.cursor_get curs b "" Mdb._SET in
           Mdb.cursor_del curs 0;
          ) !l
       )
    )


exception Unrecord_dependencies of string*string
let unrecord txn alldb patch patch_id=
  with_cursor
    txn alldb.dbi_patches
    (fun curs->
     try
       let rec iterate a b=
         if a=patch_id then (
           if branch_has_patch txn alldb alldb.current_branch b then raise (Unrecord_dependencies (to_hex patch_id,to_hex b))
           else
             let a,b=Mdb.cursor_get curs a b Mdb._NEXT in
             iterate a b
         )
       in
       let a,b=Mdb.cursor_get curs patch_id "" Mdb._SET in
       iterate a b
     with Mdb.Notfound->()
    );
  unsafe_unrecord txn alldb patch patch_id;



exception Dependency_not_met of string*string
let apply txn alldb patch patch_id=
  with_cursor
    txn alldb.dbi_branches
    (fun curs->
     let applicable=
       try let _=Mdb.cursor_get curs alldb.current_branch ("\003"^patch_id) Mdb._GET_BOTH in false
       with
         Mdb.Notfound->
         begin
           try let _=Mdb.cursor_get curs alldb.current_branch ("\001"^patch_id) Mdb._GET_BOTH in false
           with Mdb.Notfound->true
         end
     in
     if applicable then
       begin
         List.iter
           (fun dep->
            if not (branch_has_patch txn alldb alldb.current_branch dep) then
              raise (Dependency_not_met (to_hex patch_id,to_hex dep))
            else
              Mdb.put txn alldb.dbi_patches dep patch_id 0;
           ) patch.dependencies;
         Mdb.put txn alldb.dbi_branches alldb.current_branch ("\001"^patch_id) 0;
         unsafe_apply txn alldb patch patch_id
       end
    )


let write_changes ~pijuldir txn alldb=
  let file,o=Filename.open_temp_file ~mode:[Open_binary;Open_creat;Open_trunc]
                                     ~temp_dir:pijuldir "changes" "" in
  (try
      with_cursor
        txn alldb.dbi_branches
        (fun curs->
         try
           let rec iterate a b=
             if a=alldb.current_branch then (
               output_string o b;
               let a,b=Mdb.cursor_get curs "" "" Mdb._NEXT in
               iterate a b
             )
           in
           let a,b=Mdb.cursor_get curs alldb.current_branch "" Mdb._SET in
           iterate a b
         with Mdb.Notfound->()
        );
      close_out o
    with e->(close_out o;raise e));
  let targ=changesfile alldb.current_branch in
  Sys.rename file (fc pijuldir targ)


(*** Record *)


type line={key:string;half_deleted:bool;mutable children:line list;mutable order:int;mutable spit:bool}
let empty_line={key="";half_deleted=false;children=[];order=0;spit=false}



let retrieve txn alldb key0=
  let sink={empty_line with key=""} in
  let cache=ref (M.singleton "" sink) in
  let rec retrieve curs key=
    try
      M.find key !cache
    with
      Not_found->
      begin
        let key=with_cursor txn alldb.dbi_nodes (fun curs->fst (Mdb.cursor_get curs key "" Mdb._SET_RANGE)) in
        let half_deleted=
          try
            neighbors_iter
              txn alldb key
              (fun _->raise Found)
              (str (parent_flag lor deleted_flag));
            false
          with
            Found->true
        in
        let e={empty_line with key;half_deleted} in
        cache:=M.add key e !cache;

        pseudo_children_iter
          txn alldb key
          (fun b->
           let b=node_of_val b in
           e.children<-(retrieve curs b)::(e.children)
          );
        let _=
          match e.children with
            []->(
            (* log "%S no children\n"(to_hex e.key); *)
            e.children<-[sink]
          )
          | _->()
        in
        e
      end
  in
  with_cursor txn alldb.dbi_nodes (fun curs->retrieve curs key0)


module S2=Set.Make(struct type t=string*string let compare=compare end)
(* topological_order is not really a topological sort, since there might be cycles.
Therefore, it is an order in which nodes on a cycle get the same number, nodes closer to the root get smaller numbers, nodes down a cycle get larger numbers (anyway, cycles are conflicts). *)
let topological_order line=
  let trans=ref S2.empty in
  let rec transitive line=
    if line.spit then S.singleton line.key else
      begin
        line.spit<-true;
        let all_descendants=
          List.fold_left (fun m chi->S.union m (transitive chi)) S.empty line.children
        in
        S.iter (fun k->trans:=S2.add (line.key,k) !trans) all_descendants;
        S.add line.key all_descendants
      end
  in
  let _=transitive line in
  let rec topological_order maxlevel level line=
    line.order<-max line.order level;
    line.spit<-false;
    List.fold_left (fun m chi->
                    topological_order m (if S2.mem (chi.key,line.key) !trans then level else (level+1)) chi
                   ) (max line.order maxlevel) line.children
  in
  let m=topological_order 0 0 line in
  m





(** outputs a file to the given channel. The optional parameter [store] is meant for {!diff}, which needs to remember what it output. *)
let output_file ?(store=false) ouch file=
  let maxlevel=topological_order file in
  let a=Array.make (maxlevel+1) [] in
  let rec fill_topo line=
    if not line.spit then (
      line.children<-List.sort (fun a b->compare a.order b.order) line.children;
      a.(line.order)<-line::a.(line.order);
      if line.half_deleted then
        a.(line.order)<-line::a.(line.order);
      line.spit<-true;
      List.iter (fun chi->fill_topo chi) line.children
    )
  in
  fill_topo file;
  let rec complete_levels line s=
    if not (S.mem line.key s) then (
      List.fold_left
        (fun s chi->
         let _=
           match a.(line.order) with
             [_] | []->()
             | _->
                begin
                  for i=line.order+1 to chi.order-1 do
                    a.(i)<-empty_line :: a.(i)
                  done;
                end;
         in
         complete_levels chi s
        ) (S.add line.key s) line.children
    ) else
      s
  in
  let _=complete_levels file S.empty in

  let rec reset_spit t s=
    if not (S.mem t.key s) then (
      t.spit<-false;
      List.iter (fun c->reset_spit c (S.add t.key s)) t.children
    )
  in
  reset_spit file S.empty;
  let arra=ref [] in
  let output_line k cont=
    if store then arra:=k:: !arra;
    output_string ouch cont;
  in
  let rec output_file i=
    (* log "output_file %d\n" i; *)
    if i<Array.length a then
      begin
        (* Printf.eprintf "output_file %d %d\n" i (List.length a.(i));flush stderr; *)
        match a.(i) with
          []->output_file (i+1)
        | [h]->
           begin
             let k=h.key in
             (* Printf.eprintf "output file : %S\n" (to_hex k); *)
             if String.length k>key_size then (
               let cont=String.sub k key_size (String.length k-key_size) in
               (* Printf.eprintf "output file : %S\n" cont; *)
               output_line (String.sub k 0 key_size) cont;
               h.spit<-true;
             );
             output_file (i+1)
           end
        | l->
           begin
             output_line "" ">>>>>>>>>>>>>>>>>>>>>>>>>>>>\n";
             (* Ici, on veut toutes les possibilités. *)
             let first=ref true in
             let next=ref 0 in
             let rec output_conflict st l=
               next:=max !next l.order;
               match a.(l.order) with
                 []
               | [_]->
                  List.iter
                    (fun h->
                     if not !first then
                       output_line "" "==========================\n"
                     else first:=false;
                     let k=h.key in
                     let cont=String.sub k hash_size (String.length k-hash_size) in
                     output_line k cont;
                    ) (List.rev st);
               | _->
                  List.iter (fun ll->if ll.order>l.order then output_conflict (l::st) ll) l.children
             in
             List.iter (output_conflict []) l;
             output_line "" "<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
             output_file !next
           end
      end
  in
  output_file 1;
  List.rev !arra




let diff txn alldb line_num a b=
  if not (Sys.file_exists b) then [] else
    begin
      (* Printf.fprintf stderr "diff %S\n" b;flush stderr; *)
      let inp,out=Unix.open_process (Printf.sprintf "diff - %s" b) in
      let arra=output_file ~store:true out a in
      close_out out;
      let arra=Array.of_list (arra) in

      let rec prev i=if i<0 then a.key else if arra.(i)="" then prev (i-1) else arra.(i) in
      let rec next i=if i>=Array.length arra then "" else if arra.(i)="" then next (i+1) else arra.(i) in

      let lines=ref [] in
      let l=ref "" in
      (try
          while true do
            l:=input_line inp;
            (* Printf.eprintf "diff says %S\n" !l;flush stderr; *)
            if !l.[0]<>'\\' then lines:= !l:: (match !lines with []->[] | h::s->(h^"\n")::s)
          done
        with End_of_file->());
      if !l<>"" then
        if !l.[0]<>'\\' then lines:= !l:: (match !lines with []->[] | h::s->(h^"\n")::s);
      let lines=Array.of_list (List.rev !lines) in
      let rec make_hunks i act=
        if i>=Array.length lines then act else
          if Str.string_match (Str.regexp "\\([0-9,]*\\)\\([adc]\\)\\([0-9,]*\\)") lines.(i) 0 then
            let (u,u'),(v,v'),change=
              let range x=
                match Str.split (Str.regexp ",") x with
                  u::v::_->(int_of_string u,int_of_string v)
                | u::_->let uu=int_of_string u in uu,uu
                | _->failwith "range"
              in
              let a=Str.matched_group 1 lines.(i) in
              let b=Str.matched_group 3 lines.(i) in
              range a,range b,Str.matched_group 2 lines.(i)
            in

            let make_add i=
              let rec make_keys context j keys=
                if j>i+(v'-v)+1 then context,keys else
                  begin
                    let l=create_num !line_num in
                    incr line_num;
                    let cont=lines.(j) in
                    let cont=String.sub cont 2 (String.length cont-2) in
                    (* cont.[0]<-'\000'; *)
                    make_keys l (j+1) ((context,(str new_flag)^l^cont)::keys)
                  end
              in
              let keys=
                let cont,keys=make_keys (prev (u-1)) (i+1) [] in
                let n=next u' in
                if n="" then keys else ((cont,(str 0)^n)::keys)
              in
              List.rev keys
            in
            let make_del ()=
              let rec make_del i del_edges=
                if i>=u' then del_edges
                else make_del (i+1) (delete_edges txn alldb arra.(i) del_edges)
              in
              make_del (u-1) []
            in
            if change="d" then
              make_hunks (i+u'-u+1) (Minus (make_del ())::act)
            else if change="a" then
              make_hunks (i+v'-v+1) (Plus (make_add i)::act)
            else if change="c" then
              make_hunks (i+u'-u+v'-v+3) (Minus (make_del())::Plus (make_add (i+u'-u+1))::act)
            else
              make_hunks (i+1) act
          else
            make_hunks (i+1) act
      in
      make_hunks 0 []
    end


let curs_iter curs f=
  try
    let a,b=Mdb.cursor_get curs "" "" Mdb._FIRST in
    f a b;
    while true do
      let a,b=Mdb.cursor_get curs "" "" Mdb._NEXT in
      f a b
    done;
  with Mdb.Notfound->()



exception Already_in_repository of string
(* Créer tous les inodes sur le chemin, s'il y a besoin. *)
let add_inode ?(inode="") txn alldb f=
  let rec dfs curs dir path=
    match path with
      []->()
    | h::s->
       begin
         let next=
           try
             Mdb.get txn alldb.dbi_tree (dir^h)
           with
             Mdb.Notfound->
             begin
               let inode=
                 match s with
                   [] when inode<>""->inode
                 | _->create_new_inode curs
               in
               (* Mdb.put txn alldb.dbi_filenames inode h 0; *)
               Mdb.put txn alldb.dbi_tree (dir^h) inode 0;
               (* Printf.eprintf "revtree put: %S %S %S\n" (to_hex inode) (to_hex dir) h;flush stderr; *)
               Mdb.put txn alldb.dbi_revtree inode (dir^h) 0;
               let _=
                 match s with
                   []->()
                 | _::_->Mdb.put txn alldb.dbi_tree inode "" 0
                                 (* Indiquer que c'est un répertoire. *)
               in
               inode
             end
         in
         dfs curs next s
       end
  in
  with_cursor
    txn alldb.dbi_revtree
    (fun curs->dfs curs root_inode f)

let addfile txn alldb f=
  add_inode ~inode:"" txn alldb f


let get_inode txn alldb f=
  let rec dfs parent dir path=
    match path with
      []->parent,dir
    | h::s->(
      (* Printf.eprintf "get_inode %S %S\n" (to_hex dir) h;flush stderr; *)
       let next=Mdb.get txn alldb.dbi_tree (dir^h) in
       dfs dir next s
    )
  in
  dfs root_inode root_inode f



exception Not_in_repository of string
let delfile txn alldb f=
  try
    let _,inode=get_inode txn alldb f in
    let v=Mdb.get txn alldb.dbi_inodes inode in
    v.[0]<-'\002';
    Mdb.put txn alldb.dbi_inodes inode v 0
  with
    Mdb.Notfound->raise (Not_in_repository (String.concat "/" f))



let movefile txn alldb f f'=
  try
    let parent,inode=get_inode txn alldb f in
    (* Printf.fprintf stderr "movefile : %S\n" (to_hex inode);flush stderr; *)
    let basename=List.hd (List.rev f) in
    with_cursor
      txn alldb.dbi_tree
      (fun curs->
       let _=Mdb.cursor_get curs (parent^basename) "" Mdb._SET in
       Mdb.cursor_del curs 0);
    add_inode ~inode txn alldb f';
    (* Printf.eprintf "movefile? %S\n" (to_hex inode);flush stderr; *)
    let v=Mdb.get txn alldb.dbi_inodes inode in
    (* Printf.eprintf "movefile %S %S\n" (to_hex inode) (to_hex v);flush stderr; *)
    v.[0]<-'\001';
    Mdb.put txn alldb.dbi_inodes inode v 0
  with
    Mdb.Notfound->raise (Not_in_repository (String.concat "/" f))



let inode_iter txn alldb inode f=
  with_cursor
    txn alldb.dbi_tree
    (fun curs->
     let rec iter k v=
       (* Printf.eprintf "inode_iter %S %S\n" (to_hex k) (to_hex inode);flush stderr; *)
       if streq k 0 inode 0 inode_size=0 then
         (f k v;
          let k,v=Mdb.cursor_get curs k v Mdb._NEXT in
          iter k v)
     in
     try
       let k,v=Mdb.cursor_get curs inode "" Mdb._SET_RANGE in
       iter k v
     with
       Mdb.Notfound->()
    )

let tree_iter txn rep acc0 f=
  let rec fold inode acc=
    inode_iter
      txn rep inode
      (fun k v->
       let acc'=f acc (String.sub k inode_size (String.length k-inode_size)) in
       fold v acc'
      )
  in
  fold root_inode acc0


let record ~working_copy txn alldb=
  let actions=ref [] in
  let line_num=ref 0 in
  let extra_nodes=ref (M.singleton root_inode root_key) in
  let updatables=ref M.empty in
  let rec dfs curs parent cur path real_path basename=
    (* let is_directory=try (Mdb.get txn alldb.dbi_inodes cur) = "" with Mdb.Notfound->false in *)
    if cur<>root_inode then
      begin
        (* Printf.eprintf "dfs %S %S\n" real_path basename;flush stderr; *)
        (* Est-ce que ce fichier est une addition ? *)
        let parent_node=
          try let p=Mdb.get txn alldb.dbi_inodes parent in
              String.sub p 3 key_size
          with Mdb.Notfound->M.find parent !extra_nodes
        in
        try
          let node=Mdb.get txn alldb.dbi_inodes cur in
          (* Printf.eprintf "record node=%S\n" (to_hex node);flush stderr; *)
          let permissions=
            let perm=(Unix.stat real_path).Unix.st_perm in
            let p=String.create 2 in
            p.[0]<-char_of_int ((perm lsr 8) land 0xff);
            p.[1]<-char_of_int (perm land 0xff);
            p
          in
          if node.[0]='\000' && permissions<>String.sub node 1 2 then node.[0]<-'\001';
          match node.[0] with
            '\001'->
            begin
              (* Ce noeud a été déplacé par rapport au graphe. *)
              let l1=create_num !line_num in
              incr line_num;
              let l2=String.sub node 3 key_size in
              (* Printf.eprintf "1. l1,l2=%S %S\n" (to_hex l1) (to_hex l2);flush stderr; *)
              (* Printf.eprintf "1. parent=%S\n" (to_hex parent_node);flush stderr; *)
              (* Printf.eprintf "cur=%S\n" (to_hex cur);flush stderr; *)
              let ret=retrieve txn alldb l2 in
              let dif=
                if Sys.is_directory real_path then
                  []
                else
                  diff txn alldb line_num ret real_path
              in
              let deleted_other_names=
                let edges=ref [] in
                folder_parents_iter
                  txn alldb l2
                  (fun parent->
                   let par=node_of_val parent in
                   parent.[0]<-change_direction parent.[0];
                   String.blit l2 0 parent (1+hash_size) key_size;
                   edges:=(par,parent):: !edges;
                   folder_parents_iter
                     txn alldb par
                     (fun grandparent->
                      let grpar=node_of_val grandparent in
                      grandparent.[0]<-change_direction grandparent.[0];
                      String.blit par 0 grandparent (1+hash_size) key_size;
                      edges:=(grpar,grandparent):: !edges;
                     )
                  );
                !edges
              in
              actions:=
                Plusfile (path,[l1,str (folder_flag) ^ l2;
                                parent_node,str (new_flag lor folder_flag) ^l1^permissions^basename])
                ::Minusfile ("-"^path, deleted_other_names)
                ::dif
                @ !actions;
            end
          | '\002'->
             begin
              let l2=String.sub node 3 key_size in
              let deleted_other_names=
                let edges=ref [] in
                folder_parents_iter
                  txn alldb l2
                  (fun parent->
                   let par=node_of_val parent in
                   parent.[0]<-change_direction parent.[0];
                   String.blit l2 0 parent (1+hash_size) key_size;
                   edges:=(par,parent):: !edges;
                   folder_parents_iter
                     txn alldb par
                     (fun grandparent->
                      let grpar=node_of_val grandparent in
                      grandparent.[0]<-change_direction grandparent.[0];
                      String.blit par 0 grandparent (1+hash_size) key_size;
                      edges:=(grpar,grandparent):: !edges;
                     )
                  );
                !edges
              in
              actions:=Minusfile ("-"^path, deleted_other_names) :: !actions
             end
          | _->
             if Sys.is_directory real_path then
               ()
             else (
               let ret=retrieve txn alldb (String.sub node 3 key_size) in
               let dif=diff txn alldb line_num ret real_path in
               actions:=dif @ !actions;
             )
        with
          Mdb.Notfound->
          begin
            let l1=create_num !line_num in
            incr line_num;
            let l2=create_num !line_num in
            incr line_num;
            (* Printf.eprintf "nf. l1,l2=%S %S\n" (to_hex l1) (to_hex l2);flush stderr; *)
            (* Printf.eprintf "cur=%S\n" (to_hex cur);flush stderr; *)
            extra_nodes:=M.add cur l2 !extra_nodes;
            updatables:=M.add l2 cur !updatables;
            let dif=
              if Sys.is_directory real_path then
                []
              else (
                let ret={empty_line with key=l2} in
                List.concat
                  (List.map (function Plus h->h | Minus _ | Plusfile _ | Minusfile _->assert false)
                            (diff txn alldb line_num ret real_path))
              )
            in
            let permissions=
              let perm=(Unix.stat real_path).Unix.st_perm in
              let p=String.create 2 in
              p.[0]<-char_of_int ((perm lsr 8) land 0xff);
              p.[1]<-char_of_int (perm land 0xff);
              p
            in
            actions:=
              Plusfile (path,
                        (l1,str (new_flag lor folder_flag) ^ l2)
                        ::(parent_node,str (new_flag lor folder_flag) ^l1^permissions^basename)
                        ::dif)
              :: !actions;
          end
      end;
    inode_iter
      txn alldb cur
      (fun dir inode->
       if inode<>"" then
         let basename=String.sub dir inode_size (String.length dir-inode_size) in
         dfs curs cur inode (if path<>"" then path^"/"^basename else basename)
             (Filename.concat real_path basename) basename
      );
  in
  with_cursor
    txn alldb.dbi_nodes
    (fun curs->dfs curs root_inode root_inode "" working_copy "");
  !actions, !updatables



let sync_files txn alldb patch patch_id updates=
  List.iter
    (fun record->
     match record with
       Plusfile (_,(_,h)::(_,kname)::_) when String.length h=1+line_size->
       begin
         let name=String.sub kname (1+line_size) (String.length kname-1-line_size) in
         let l2=String.sub h 1 line_size in
         let p2=patch_id^l2 in
         let inode_l2=try M.find l2 updates with Not_found->with_cursor txn alldb.dbi_revtree create_new_inode in
         let permissions=String.sub name 0 2 in
         (* Fichier local, il existe dans trees, mais pas dans inodes. *)
         Mdb.put txn alldb.dbi_inodes inode_l2 ("\000"^permissions^p2) 0;
         Mdb.put txn alldb.dbi_revinodes p2 inode_l2 0;
       end
     | Minusfile (_,h)->
        begin
          (* Printf.eprintf "sync_files\n"; *)
          List.iter (fun (_,b)->
                     let key=String.sub b (1+hash_size) key_size in
                     (* Printf.eprintf "inode %S\n" (to_hex key);flush stderr; *)
                     (* b peut contenir le noeud du nom, ou le noeud de l'inode. Dans le premier cas, le premier appel lève Mdb.Notfound. *)
                     try
                       let inode=Mdb.get txn alldb.dbi_revinodes key in

                       (* Décider si on doit le supprimer, c'est-à-dire si personne ne pointe plus sur lui dans le graphe. *)
                       let has_parents=
                         try
                           folder_parents_iter
                             txn alldb key
                             (fun _->raise Found);
                           false
                         with Found->true
                       in
                       if not has_parents then
                         begin
                           (* Printf.eprintf "inode=%S\n" (to_hex inode);flush stderr; *)
                           let del_keys=ref [] in
                           let del_inodes=ref [] in
                           let rec recursive_del inode=
                             (* Printf.eprintf "deleting inode %S\n" (to_hex inode);flush stderr; *)
                             if inode<>"" then
                               begin
                                 del_inodes:=inode:: !del_inodes;
                                 inode_iter
                                   txn alldb inode
                                   (fun k child->
                                    del_keys:=k :: !del_keys;
                                    recursive_del child
                                   )
                               end
                           in
                           recursive_del inode;
                           let parent=Mdb.get txn alldb.dbi_revtree inode in
                           Mdb.del txn alldb.dbi_tree parent None;
                           List.iter (fun a->Mdb.del txn alldb.dbi_tree a None) !del_keys;
                           List.iter (fun a->Mdb.del txn alldb.dbi_revtree a None) !del_inodes;
                         end
                     with
                       Mdb.Notfound->()
                    ) h
        end
     | Plusfile _ | Plus _ | Minus _->()
    ) patch.records



let save_patch ~patches_path patch=
  let f,o=Filename.open_temp_file ~mode:[Open_binary;Open_creat;Open_trunc] ~temp_dir:patches_path "pijul" "" in
  output_patch o patch;
  close_out o;
  let digest=sha1 f in
  let patchname=fc patches_path (to_hex digest) in
  Unix.rename f patchname;
  digest



let debug_repository o txn alldb=
  let styles=Array.make 16 "" in
  for i=0 to Array.length styles-1 do
    styles.(i)<-"color="^[|"red";"blue";"green";"black"|].((i lsr 1) land 3);
    if is_deleted (c i) then styles.(i)<-styles.(i)^",style=dashed"
    else (if is_pseudo (c i) then styles.(i)<-styles.(i)^",style=dotted")
  done;
  Printf.fprintf o "digraph{\n";
  let conts=ref S.empty in
  with_cursor
    txn alldb.dbi_nodes
    (fun curs->
     curs_iter
       curs
       (fun a b->
        (* Printf.eprintf "debug a: %S\n      b: %S\n" (to_hex a) (to_hex b); flush stderr; *)
        let aa=String.sub a 0 key_size in
        let acont=String.sub a key_size (String.length a-key_size) in
        let bb=node_of_val b in
        let flag=int_of_char b.[0] in
        if not (S.mem aa !conts) then (
          conts:=S.add aa !conts;
          Printf.fprintf o "n_%s[label=%S];\n" (to_hex aa) (to_hex aa ^ ":"^acont);
        );
        Printf.fprintf o "n_%s->n_%s [%s,label=\"%d\"];\n" (to_hex aa) (to_hex bb) styles.(flag land 15) flag
       )
    );
  (* log "debug, tracked files:\n"; *)
  with_cursor
    txn alldb.dbi_tree
    (fun curs->
     curs_iter
       curs
       (fun a b->
        (* Printf.fprintf stderr "%S %S\n" (to_hex a) (to_hex b);flush stderr; *)
        let aa=String.sub a 0 (min inode_size (String.length a)) in
        let bb=String.sub b 0 (min inode_size (String.length b)) in
        Printf.fprintf o "f_%s[label=%S];\n" (to_hex bb) (String.sub a inode_size (max 0 (String.length a-inode_size)));
        Printf.fprintf o "f_%s->f_%s;\n" (to_hex aa) (to_hex bb)
       )
    );
  with_cursor
    txn alldb.dbi_inodes
    (fun curs->
     curs_iter
       curs
       (fun a b->
        Printf.fprintf o "//inodes: %S %S\n" (to_hex a) (to_hex b);flush stderr;
        (* let aa=String.sub a 0 (min inode_size (String.length a)) in *)
        (* let bb=String.sub b 0 (min inode_size (String.length b)) in *)
        (* Printf.fprintf o "f_%s[label=%S];\n" (to_hex bb) (String.sub a inode_size (max 0 (String.length a-inode_size))); *)
        (* Printf.fprintf o "f_%s->f_%s;\n" (to_hex aa) (to_hex bb) *)
       )
    );
  with_cursor
    txn alldb.dbi_revinodes
    (fun curs->
     curs_iter
       curs
       (fun a b->
        Printf.fprintf o "//revinodes: %S %S\n" (to_hex a) (to_hex b);flush stderr;
        (* let aa=String.sub a 0 (min inode_size (String.length a)) in *)
        (* let bb=String.sub b 0 (min inode_size (String.length b)) in *)
        (* Printf.fprintf o "f_%s[label=%S];\n" (to_hex bb) (String.sub a inode_size (max 0 (String.length a-inode_size))); *)
        (* Printf.fprintf o "f_%s->f_%s;\n" (to_hex aa) (to_hex bb) *)
       )
    );

  with_cursor
    txn alldb.dbi_revtree
    (fun curs->
     curs_iter
       curs
       (fun a b->
        (* Printf.fprintf stderr "%S %S\n" (to_hex a) (to_hex b);flush stderr; *)
        let aa=String.sub a 0 (min inode_size (String.length a)) in
        let bb=String.sub b 0 (min inode_size (String.length b)) in
        Printf.fprintf o "f_%s->f_%s[style=dashed];\n" (to_hex aa) (to_hex bb)
       )
    );
  Printf.fprintf o "}\n"



let unsafe_output_repository ~working_copy do_output_files txn alldb=
  let cache=ref M.empty in
  let paths=ref M.empty in
  (* Walk down the graph, collecting files, and look them up using the revinodes base (possibly adding new files to that table). *)
  let rec retrieve_paths key path inode inode_path=
    if not (M.mem key !cache) then
      begin
        cache:=M.add key () !cache;
        folder_children_iter
          txn alldb key
          (fun b->
           try
             let bv=node_of_val b in
             let cont_b=
               with_cursor txn alldb.dbi_nodes
                           (fun curs->
                            let k,_=Mdb.cursor_get curs bv "" Mdb._SET_RANGE in
                            if streq k 0 bv 0 key_size <> 0 then raise Mdb.Notfound
                            else String.sub k key_size (String.length k-key_size))
             in
             let filename=String.sub cont_b 2 (String.length cont_b-2) in
             let perms=((int_of_char cont_b.[0]) lsl 8) lor (int_of_char cont_b.[1]) in
             let path'=if path="" then filename else (path^"/"^filename) in
             folder_children_iter
               txn alldb bv
               (fun c->
                let cv=node_of_val c in
                let c_is_file=
                  try
                    neighbors_iter txn alldb cv (fun _->raise Found) (str 0);
                    neighbors_iter txn alldb cv (fun _->raise Found) (str (pseudo_flag));
                    neighbors_iter txn alldb cv (fun _->raise Found) (str (deleted_flag));
                    neighbors_iter txn alldb cv (fun _->raise Found) (str (deleted_flag lor pseudo_flag));
                    false
                  with
                    Found->true
                in
                let inode'=try
                    Mdb.get txn alldb.dbi_revinodes cv
                  with
                    Mdb.Notfound->
                    begin
                      (* New file. *)
                      let inode'=with_cursor txn alldb.dbi_revtree create_new_inode in
                      Mdb.put txn alldb.dbi_inodes inode' ("\000"^String.sub cont_b 0 2^cv) 0;
                      Mdb.put txn alldb.dbi_revinodes cv inode' 0;
                      Mdb.put txn alldb.dbi_tree (inode^filename) inode' 0;
                      Mdb.put txn alldb.dbi_revtree inode' (inode^filename) 0;
                      inode'
                    end
                in
                paths:=M.add path' ((cont_b,cv,inode',perms,c_is_file)::(try M.find path' !paths with Not_found->[])) !paths;
                if not c_is_file then (
                  let basename=Mdb.get txn alldb.dbi_revtree inode' in
                  let basename=String.sub basename inode_size (String.length basename-inode_size) in
                  retrieve_paths cv path' inode' (Filename.concat inode_path basename);
                )
               );
           with Mdb.Notfound->()
          );
      end
  in
  Mdb.drop txn alldb.dbi_tree false;
  retrieve_paths root_key "" root_inode "";
  (* Then output the files. The main challenge here, is that maybe a succession of patches moved several elements of the path to a single file, i.e. x/a became y/b, so we need to take care of this when moving existing files. *)
  M.iter
    (fun k a->
     (* k is now a path, and a is a list of nodes that are supposed to be at that path (of course more than one file in a is a conflict. *)
     let output_file path file=
       let o=open_out path in
       try
         let _=output_file o file in
         close_out o
       with e->(close_out o;raise e)
     in
     let output basename key inode perms is_file suffix=
       let rec path_for_inode inode=
         try
           let next=Mdb.get txn alldb.dbi_revtree inode in
           let inode'=String.sub next 0 inode_size in
           let basename=String.sub next inode_size (String.length next-inode_size) in
           Filename.concat (path_for_inode inode') basename
         with
           Mdb.Notfound->""
       in
       let fi=path_for_inode inode in
       let former=fc working_copy fi in
       let newer=(fc working_copy k)^suffix in

       let parent=Mdb.get txn alldb.dbi_revtree inode in
       let parent=String.sub parent 0 inode_size in
       let b=String.sub basename 2 (String.length basename-2) in
       Mdb.put txn alldb.dbi_tree (parent ^ b) inode 0;
       Mdb.put txn alldb.dbi_revtree inode (parent ^ b) 0;

       if is_file then
         begin
           let _=Sys.command ("mkdir -p "^(Filename.quote (Filename.dirname newer))) in
           let _=try Sys.rename former newer with Sys_error _->() in
           if do_output_files then (
             output_file newer (retrieve txn alldb key);
             Unix.chmod newer perms
           );
         end
       else
         begin
           (try Sys.rename former newer with Sys_error _->
              let _=Sys.command ("mkdir -p "^newer) in ());
           Unix.chmod newer perms
         end
     in
     match a with
       []->()
     | [basename,key,inode,perms,is_file]->output basename key inode perms is_file ""
     | _->
        let _=List.fold_left (fun i (basename,key,inode,perms,is_file)->output basename key inode perms is_file (Printf.sprintf "~%d" i);i+1) 0 a in
        ()
    ) !paths


let output_repository ~working_copy env txn alldb records=
  unsafe_output_repository ~working_copy false txn alldb;
  let t=Mdb.txn_begin env (Some txn) 0 in
  try
    let patch={empty_patch with records} in
    let patchid=String.make hash_size '\000' in
    unsafe_apply t alldb patch patchid;
    unsafe_output_repository ~working_copy true txn alldb;
    Mdb.txn_abort t;
  with
    e->(Mdb.txn_abort t;raise e)


let patches_topo patchespath patches=
  let rp=ref [] in
  let rec treat m h=
    if S.mem h m then (
      let deps=
        let o=open_in_bin (Filename.concat patchespath (to_hex h)) in
        let p=input_dependencies o in
        close_in o;
        p
      in
      let m'=List.fold_left treat (S.remove h m) deps in
      rp:=h :: !rp;
      m'
    ) else m
  in
  let rec iterate m=
    if not (S.is_empty m) then
      iterate (treat m (S.min_elt m))
  in
  iterate (List.fold_left (fun m h->S.add h m) S.empty patches);
  !rp
