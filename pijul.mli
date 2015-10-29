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
(** Handling repositories. General remark: all patch numbers taken by these functions (except {!from_hex}) are "raw" patch numbers, although the file names of patches are their hexadecimal encoding. *)

(** {2 Repositories} *)

type repository={ current_branch:string; dbi_nodes:Mdb.dbi; dbi_patches:Mdb.dbi; dbi_branches: Mdb.dbi; dbi_tree: Mdb.dbi; dbi_revtree:Mdb.dbi; dbi_inodes: Mdb.dbi; dbi_revinodes:Mdb.dbi }

val _pijul : string
val pijuldir : string -> string
val patchesdir : string -> string
val pristinedir : string -> string
val changesfile : string -> string
(** Use this function in conjunction with {!with_repo}:

[with_repo "_pijul" (fun _ txn -> open_repository txn)]
*)
val open_repository : Mdb.txn -> repository

(** The name of the default branch. *)
val default_branch : string

(** The "current" branch of a repository. Switching would involve unapplying and re-applying patches (not implemented yet). *)
val current_branch : repository -> string

(** Tells whether a branch has a patch: [branch_has_patch txn repository branch path] *)
val branch_has_patch : Mdb.txn -> repository -> string -> string -> bool

val branch_patches : Mdb.txn -> repository -> string -> string list

(** {2 Patches} *)

type record =
    Plusfile of string * (string * string) list
  | Minusfile of string * (string * string) list
  | Plus of (string * string) list
  | Minus of (string * string) list


type patch = {
           records : record list;
           dependencies : string list;
           (** Dependencies of this patch, in raw encoding (i.e. not in hexadecimal). *)
           obsoletes : string list;
           (** What patches are superseded by this patch (also raw encoding). *)
           name : string;
           (** Patch name, including long comment (separated by \n). *)
           author : string;
           time : string;
           (** The output of "date -In", i.e. iso 8601 dates at the nanosecond. *)
         }
val key_size : int
val empty_patch : patch
val input_patch : in_channel -> patch

val input_dependencies : in_channel -> string list

val input_time : in_channel -> string

(** Example: [save_patch ~tech_path:"repository/_pijul" patch]. *)
val save_patch : patches_path:string -> patch -> string

(** {2 Applying and unapplying patches} *)

(** Apply without checking for dependencies or obsolete patches. *)
val unsafe_apply : Mdb.txn -> repository -> patch -> string -> unit

exception Dependency_not_met of string * string
val apply : Mdb.txn -> repository -> patch -> string -> unit

val write_changes : pijuldir:string -> Mdb.txn -> repository -> unit

(** The graph side of unrecording: works exclusively on the nodes. Leaves files unsynchronized. *)
val unsafe_unrecord : Mdb.txn -> repository -> patch -> string -> unit

exception Unrecord_dependencies of string*string

(** Unrecord, checking for dependencies and obsolete patches first. Leaves files unsynchronized. *)
val unrecord : Mdb.txn -> repository -> patch -> string -> unit

(** Synchronize files after unrecording a patch. *)
val unrecord_sync : Mdb.txn -> repository -> string -> unit

(** {2 Making patches} *)

module M:Map.S  with type key = string

val record : working_copy:string -> Mdb.txn -> repository -> record list * string M.t

(** After a record (and apply), synchronize the newly recorded files with the graph, as new node identifiers have been created for them. *)
val sync_files : Mdb.txn -> repository -> patch -> string -> string M.t -> unit
(** Computes the dependencies of a list of records. *)
val dependencies : record list -> string list

(** Computes the "inverse" of a list of records. Note: applying the inverse of p does not necessarily leave the repository in the same state as before applying p. There is an issue with "equivalent" edges. *)
val rollback : Mdb.txn -> repository -> record list -> string -> record list


(** {2 File changes} *)

exception Already_in_repository of string

(** Adds a file to the working copy of a repository, given as a list of basenames (example: "/home/pe" would be given as ["home";"pe"]). Creates inodes and updates the "file system" tables. Does not change the graph. *)
val addfile : Mdb.txn -> repository -> string list -> unit
exception Not_in_repository of string

(** Removes a file from the tracked files of the repository, given as a list of basenames (example: "/home/pe" would be given as ["home";"pe"]). Does not change the graph. *)
val delfile : Mdb.txn -> repository -> string list -> unit

(** Change the name of a file, given as lists of basenames (example: "/home/pe" would be given as ["home";"pe"]). *)
val movefile : Mdb.txn -> repository -> string list -> string list -> unit

val tree_iter : Mdb.txn -> repository -> 'a -> ('a -> string -> 'a) -> unit


(** {2 Outputing the repository} *)

(** Outputs a version of the repository on the specified [out_channel] in the dot language, and prints information on the file system tables as comments in dot. *)
val debug_repository : out_channel -> Mdb.txn -> repository -> unit

(** Outputs the repository without any mercy for unrecorded changes. *)
val unsafe_output_repository : working_copy:string -> bool -> Mdb.txn -> repository -> unit

(** Outputs the repository, saving unrecorded changes (given in the patch) first. *)
val output_repository :
  working_copy:string -> Mdb.env -> Mdb.txn -> repository -> record list -> unit


(** {2 High level auxiliary functions} *)

(** Converts a string to hexadecimal. *)
val to_hex : string -> string

(** Reads a string in hexadecimal. *)
val from_hex : string -> string

(** Opens an Mdb environment and starts an Mdb transaction, and takes care of everything in case an exception is raised. *)
val with_pijul : string -> (Mdb.env -> Mdb.txn -> 'a) -> 'a

(** Useful to read the medieval-style branch files written by {!apply} to allow [pull] and [push] to compare branches. This is useful to avoid copying the database, but rsync and compression could probably handle this better. *)
val hash_size : int

(** Pijul uses sets of strings, feel free to reuse this module. *)
module S:Set.S  with type elt = string

(** {2 Sets of patches} *)

(** Sorts a set of patches in topological order *)
val patches_topo : string -> S.elt list -> S.elt list
