exception Nothing_to_record

let now () =
  let i=Unix.open_process_in "date -In" in
  let d=input_line i in
  let _=Unix.close_process_in i in
  d


let polite_record pijul_path records update_files=
  (* let records=with_cursor txn alldb.dbi_nodes (filter records) in *)
  match records with
    []-> raise Nothing_to_record
  | _->
     begin
       let time= now () in
       (* let author=ask_author pijul_path in *)
       (* let name=ask_name () in *)
       let author="" and name="" in
       let obsoletes=[] in
       let patch=Pijul.({ records;dependencies=dependencies records; obsoletes; name; author; time }) in
       (* List.iter (fun d->Printf.eprintf "dep %S\n" (to_hex d)) patch.dependencies;flush stderr; *)
       let patches_path=Pijul.patchesdir pijul_path in
       let patchid=Pijul.save_patch ~patches_path patch in
       Pijul.with_pijul
         pijul_path
         (fun _ txn->
          let repo=Pijul.open_repository txn in
          let _=Pijul.apply txn repo patch patchid in
          Pijul.sync_files txn repo patch patchid update_files;
          Pijul.write_changes ~pijuldir:pijul_path txn repo;
          ()
         )
     end

let record pijul_path dir txn =
  let repo = Pijul.open_repository txn in
  let records,update_files=Pijul.record ~working_copy:dir txn repo in
  polite_record pijul_path records update_files


let sort_patches patches_path patches=
  List.sort
    (fun a b->
     let pa=let o=open_in_bin (Filename.concat patches_path (Pijul.to_hex (String.sub a 0 Pijul.hash_size))) in
            let t=Pijul.input_time o in
            close_in o; t
     in
     let pb=let o=open_in_bin (Filename.concat patches_path (Pijul.to_hex (String.sub b 0 Pijul.hash_size))) in
            let t=Pijul.input_time o in
            close_in o; t
     in
     compare pb pa
    )
    patches

let unrecord pijul_path =
  let patches_path=Pijul.patchesdir pijul_path in
  let patches=
    Pijul.with_pijul
      pijul_path
      (fun _ txn->
       let repo=Pijul.open_repository txn in
       Pijul.(branch_patches txn repo repo.current_branch);
      )
  in
  let patches=sort_patches patches_path patches in
  let patches=Interaction.filter_patches pijul_path false "unrecord" patches in
  let patches=Pijul.patches_topo patches_path patches in
  Pijul.with_pijul
    pijul_path
    (fun _ txn->
     let repo=Pijul.open_repository txn in
     List.iter
       (fun patch_id->
        let o=open_in (Filename.concat patches_path (Pijul.to_hex patch_id)) in
	let p=Pijul.input_patch o in
	close_in o;
	Pijul.unsafe_unrecord txn repo p patch_id;
	Pijul.unrecord_sync txn repo patch_id;
       ) patches;
     Pijul.write_changes ~pijuldir:pijul_path txn repo;
    )


let rollback pijul_path=
  let patches_path=Pijul.patchesdir pijul_path in
  let patches=
    Pijul.with_pijul
      pijul_path
      (fun _ txn->
       let repo=Pijul.open_repository txn in
       Pijul.(branch_patches txn repo repo.current_branch);
      )
  in
  let patches=sort_patches patches_path patches in
  let patches=Interaction.filter_patches pijul_path false "rollback" patches in
  let patches=Pijul.patches_topo patches_path patches in

  let rollback_records=ref [] in
  Pijul.with_pijul
    pijul_path
    (fun _ txn->
     let repo=Pijul.open_repository txn in
     List.iter
       (fun patch_id->
        let o=open_in (Filename.concat patches_path (Pijul.to_hex patch_id)) in
        let p=Pijul.input_patch o in
        close_in o;
        rollback_records:=Pijul.(rollback txn repo p.records patch_id) @ !rollback_records
       ) patches;
    );
  polite_record pijul_path !rollback_records Pijul.M.empty
