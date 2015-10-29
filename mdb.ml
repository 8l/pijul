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
type env
type txn
type dbi
type cursor

exception Notfound
let _ =
  Callback.register_exception "lmdb_not_found" Notfound;

external env_create:unit->env="caml_mdb_env_create"
external env_copy:env->string->unit="caml_mdb_env_copy"

external env_open:env->string->int->int->unit="caml_mdb_env_open"
external env_close:env->unit="caml_mdb_env_close"
external reader_check:env->int="caml_mdb_reader_check"

external txn_begin:env->txn option->int->txn="caml_mdb_txn_begin"
external txn_abort:txn->unit="caml_mdb_txn_abort"
external txn_commit:txn->unit="caml_mdb_txn_commit"

external dbi_open:txn->string->int->dbi="caml_mdb_dbi_open"
external dbi_close:env->dbi->unit="caml_mdb_dbi_close"
external drop:txn->dbi->bool->unit="caml_mdb_drop"


external put:txn->dbi->string->string->int->unit="caml_mdb_put"
external get:txn->dbi->string->string="caml_mdb_get"
external del:txn->dbi->string->string option->unit="caml_mdb_del"

external env_set_maxdbs:env->int->unit="caml_mdb_env_set_maxdbs"

external cursor_open:txn->dbi->cursor="caml_mdb_cursor_open"
external cursor_renew:txn->cursor->unit="caml_mdb_cursor_renew"
external cursor_close:cursor->unit="caml_mdb_cursor_close"

external cursor_get:cursor->string->string->int->(string*string)="caml_mdb_cursor_get"

external cursor_del:cursor->int->unit="caml_mdb_cursor_del"

external cursor_put:cursor->string->string->int->unit="caml_mdb_cursor_put"
external cursor_count:cursor->int="caml_mdb_cursor_count"

include Mdb_constants
