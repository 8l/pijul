(*
unrecord p: if q depends on p, selecting p must also select q.
push p: if p depends on q, selecting p must also select q.

This function takes care of all this:

"push mode" means include_deps=true, "unrecord mode" means include_deps=false.
 *)

let general_filter l include_deps key dep que=
  let str=String.create 1 in
  let last_asked=ref 1 in
  let rec filter actions past i sel unsel sel_deps unsel_deps=match actions with
      []->(
      if i- !last_asked > 1 then (
      );
      List.fold_left (fun l (u,v,_,_,_,_) -> if u then v::l else l) [] past
    )
    | h::s->
       let k=key h in
       let deps=dep h in
       let selected,direction=
         if if include_deps then Pijul.S.mem k sel_deps else
              List.exists (fun d->Pijul.S.mem d sel) deps
         then
           true,1
         else
           if if include_deps then List.exists (fun d->Pijul.S.mem d unsel) deps
              else Pijul.S.mem k unsel_deps
           then
             false,1
           else
             begin
               if i- !last_asked > 1 then (
                 Printf.printf "Skipping %d\n" (i- !last_asked-1);flush stdout;
               );
               que h i;
               last_asked:=i;
               flush stdout;
               str.[0]<-'\000';
               let _=Unix.read Unix.stdin str 0 1 in
               Printf.fprintf stdout "\n";
               match str with
                 "y"->true,1
               | "n"->false,1
               | "k"->false, -1
               | _->false,0
             end
       in
       if direction<0 then
         match past with
           []->filter actions past i sel unsel sel_deps unsel_deps
         | (_,u,a,b,c,d)::v->filter (u::actions) v (i-1) a b c d
       else if direction=0 then
         filter actions past i sel unsel sel_deps unsel_deps
       else
         let sel',unsel'=if selected then Pijul.S.add k sel,unsel else sel,Pijul.S.add k unsel
         and sel_deps',unsel_deps'=
           if selected then List.fold_left (fun m d->Pijul.S.add d m) sel_deps deps,unsel_deps
           else sel_deps,List.fold_left (fun m d->Pijul.S.add d m) unsel_deps deps
         in
         filter s ((selected,h,sel,unsel,sel_deps,unsel_deps)::past) (i+1) sel' unsel' sel_deps' unsel_deps'
  in
  let tcattr = if Unix.isatty Unix.stdin
	       then
		 let tcattr=Unix.tcgetattr Unix.stdin in
		 Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { tcattr with Unix.c_icanon=false };
		 Some tcattr
	       else
		 None
  in
  let f=filter l [] 1 Pijul.S.empty Pijul.S.empty Pijul.S.empty Pijul.S.empty in
  let () = match tcattr with | Some tcattr -> Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcattr | None -> () in
  f


let filter_patches pijuldir push_mode action all_patches=
  let n=List.length all_patches in
  general_filter
    all_patches
    push_mode
    (fun h->h)
    (fun h->
     let o=open_in_bin (Filename.concat (Pijul.patchesdir pijuldir) (Pijul.to_hex h)) in
     let p=Pijul.input_dependencies o in
     close_in o;
     p)
    (fun h i->
     let p=
       let o=open_in_bin (Filename.concat (Pijul.patchesdir pijuldir) (Pijul.to_hex h)) in
       let p=Pijul.input_patch o in
       close_in o;
       p
     in
     let open Pijul in
     Printf.fprintf stdout "%s %s\n  * %s\nShall I %s this patch? (%d/%d) [ynk]: " p.time p.author p.name action i n
    )
