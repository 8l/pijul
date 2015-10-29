/*
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
*/

#include <string.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <lmdb.h>

#include <errno.h>

CAMLprim value caml_mdb_env_create(){
  CAMLparam0();
  MDB_env *env;
  if(mdb_env_create(&env)){
    caml_failwith("error in mdb_env_create");
  }
  CAMLreturn(env);
}
CAMLprim value caml_mdb_env_copy(value env,value path){
  CAMLparam2(env,path);
  if(mdb_env_copy((MDB_env*)env,String_val(path))){
    caml_failwith("error in mdb_env_copy");
  }
  CAMLreturn0;
}

static inline int get_flags(const int*table, value mask_list )
{
    int c_mask = 0;
    while ( mask_list != Val_emptylist )
    {
        value head = Field(mask_list, 0);
        c_mask |= table[Long_val(head)];
        mask_list = Field(mask_list, 1);
    }
    return c_mask;
}

CAMLprim value caml_mdb_env_open(value env,value path,value flags,value mode){
  CAMLparam4(env,path,flags,mode);

  if(mdb_env_open((MDB_env*)env,String_val(path),Int_val(flags),Int_val(mode))){
    caml_failwith("error in mdb_env_open");
  }
  CAMLreturn(env);
}

CAMLprim value caml_mdb_txn_begin(value env,value parent_,value flags){
  CAMLparam3(env,parent_,flags);
  MDB_txn*parent = (parent_!=Val_int(0)) ? ((MDB_txn*)Field(parent_,0)) : NULL;
  MDB_txn*txn;
  if(mdb_txn_begin((MDB_env*)env,parent,Int_val(flags),&txn)){
    caml_failwith("error in mdb_txn_begin");
  }
  CAMLreturn(txn);
}

CAMLprim value caml_mdb_txn_abort(value txn){
  CAMLparam1(txn);
  mdb_txn_abort((MDB_txn*) txn);
  CAMLreturn0;
}
CAMLprim value caml_mdb_txn_commit(value txn){
  CAMLparam1(txn);
  if(mdb_txn_commit((MDB_txn*) txn)){
    caml_failwith("error in mdb_txn_commit");
  }
  CAMLreturn0;
}


CAMLprim value caml_mdb_dbi_open(value txn,value name,value flags){
  CAMLparam3(txn,name,flags);
  MDB_dbi dbi;
  char*str=NULL;
  if(caml_string_length(name))
    str=String_val(name);

  if(mdb_dbi_open((MDB_txn*)txn,str,Int_val(flags),&dbi)){
    caml_failwith("error in mdb_dbi_open");
  }

  CAMLreturn(Val_int(dbi));
}


CAMLprim value caml_mdb_put(value txn,value dbi,value key,value data,value flags){
  CAMLparam5(txn,dbi,key,data,flags);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  data_.mv_data=String_val(data);
  data_.mv_size=caml_string_length(data);
  if(mdb_put((MDB_txn*)txn,
             (MDB_dbi) Int_val(dbi),
             &key_,
             &data_,
             Int_val(flags)
             )){
    caml_failwith("error in mdb_put");
  }
  CAMLreturn0;
}

CAMLprim value caml_mdb_get(value txn,value dbi,value key){
  CAMLparam3(txn,dbi,key);
  CAMLlocal1(mldata);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  int ret;
  if((ret=mdb_get(  (MDB_txn*)txn,  (MDB_dbi) Int_val(dbi),  &key_,  &data_  ))){
    if(ret==MDB_NOTFOUND) {
      static value *exn=NULL;
      if(exn==NULL) exn=caml_named_value("lmdb_not_found");
      caml_raise_constant(*exn);
    } else
      caml_failwith("error in mdb_get");
  }
  mldata=caml_alloc_string(data_.mv_size);
  memcpy(String_val(mldata),data_.mv_data,data_.mv_size);
  CAMLreturn(mldata);
}


CAMLprim value caml_mdb_del(value txn,value dbi,value key,value data){
  CAMLparam4(txn,dbi,key,data);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  int ret;
  if(data ==Val_int(0)){
    if((ret=mdb_del(  (MDB_txn*)txn,  (MDB_dbi) Int_val(dbi),  &key_, NULL ))){
      if(ret==MDB_NOTFOUND) {
        static value *exn=NULL;
        if(exn==NULL) exn=caml_named_value("lmdb_not_found");
        caml_raise_constant(*exn);
      } else
        caml_failwith("error in mdb_del");
    }
  } else {
    value x=Field(data,0);
    data_.mv_data=String_val(x);
    data_.mv_size=caml_string_length(x);
    if((ret=mdb_del(  (MDB_txn*)txn,  (MDB_dbi) Int_val(dbi),  &key_, &data_ ))){
      caml_failwith("error in mdb_del");
    }
  }

  CAMLreturn0;
}



CAMLprim value caml_mdb_dbi_close(value env,value dbi){
  CAMLparam2(env,dbi);
  mdb_dbi_close((MDB_env*)env,(MDB_dbi) Int_val(dbi));
  CAMLreturn0;
}
CAMLprim value caml_mdb_drop(value txn,value dbi,value del){
  CAMLparam3(txn,dbi,del);
  if(mdb_drop((MDB_txn*)txn,(MDB_dbi) Int_val(dbi),Bool_val(del))){
    caml_failwith("error in mdb_drop");
  }
  CAMLreturn0;
}
CAMLprim value caml_mdb_env_close(value env){
  CAMLparam1(env);
  mdb_env_close((MDB_env*) env);
  CAMLreturn0;
}
CAMLprim value caml_mdb_reader_check(value env){
  CAMLparam1(env);
  int dead;
  if((mdb_reader_check( (MDB_env*)env, &dead ))){
    caml_failwith("error in mdb_reader_check");
  }
  CAMLreturn(Val_int(dead));
}
CAMLprim value caml_mdb_env_set_maxdbs(value env,value m){
  CAMLparam2(env,m);
  if(mdb_env_set_maxdbs((MDB_env*)env,Int_val(m))){
    caml_failwith("error in mdb_env_set_maxdbs");
  }
  CAMLreturn0;
}


CAMLprim value caml_mdb_cursor_open(value txn,value dbi){
  CAMLparam2(txn,dbi);
  MDB_cursor *curs;
  if(mdb_cursor_open( (MDB_txn*) txn,  (MDB_dbi) Int_val(dbi),  &curs)){
    caml_failwith("error in mdb_cursor_open");
  }
  CAMLreturn((value)curs);
}
CAMLprim value caml_mdb_cursor_renew(value txn,value cursor){
  CAMLparam2(txn,cursor);
  if(mdb_cursor_renew( (MDB_txn*) txn,  (MDB_cursor*) cursor)){
    caml_failwith("error in mdb_cursor_renew");
  }
  CAMLreturn0;
}
CAMLprim value caml_mdb_cursor_close(value curs){
  CAMLparam1(curs);
  mdb_cursor_close( (MDB_cursor*) curs );
  CAMLreturn0;
}

CAMLprim value caml_mdb_cursor_get(value curs,value key,value data,value op){
  CAMLparam4(curs,key,data,op);
  CAMLlocal3(result,mlkey,mldata);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  data_.mv_data=String_val(data);
  data_.mv_size=caml_string_length(data);

  int ret;
  if((ret=mdb_cursor_get(  (MDB_cursor*)curs,  &key_,  &data_, Int_val(op) ))){
    if(ret==MDB_NOTFOUND) {
      static value *exn=NULL;
      if(exn==NULL) exn=caml_named_value("lmdb_not_found");
      caml_raise_constant(*exn);
    } else
      caml_failwith("error in mdb_cursor_get");
  }
  mlkey=caml_alloc_string(key_.mv_size);
  memcpy(String_val(mlkey),key_.mv_data,key_.mv_size);
  mldata=caml_alloc_string(data_.mv_size);
  memcpy(String_val(mldata),data_.mv_data,data_.mv_size);
  result=caml_alloc(2,0);
  Store_field(result,0,mlkey);
  Store_field(result,1,mldata);
  CAMLreturn(result);
}

CAMLprim value caml_mdb_cursor_del(value cursor,value flags){
  CAMLparam2(cursor,flags);
  if(mdb_cursor_del((MDB_cursor*)cursor, Int_val(flags))){
    caml_failwith("error in mdb_cursor_del");
  }
  CAMLreturn0;
}

CAMLprim value caml_mdb_cursor_put(value cursor,value key,value data,value flags){
  CAMLparam4(cursor,flags,key,data);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  data_.mv_data=String_val(data);
  data_.mv_size=caml_string_length(data);
  if(mdb_cursor_put((MDB_cursor*)cursor, &key_,&data_,Int_val(flags))){
    caml_failwith("error in mdb_cursor_put");
  }
  CAMLreturn0;
}

CAMLprim value caml_mdb_cursor_count(value cursor){
  CAMLparam1(cursor);
  size_t count;
  if(mdb_cursor_count((MDB_cursor*)cursor, &count)){
    caml_failwith("error in mdb_cursor_count");
  }
  CAMLreturn(Val_int(count));
}


