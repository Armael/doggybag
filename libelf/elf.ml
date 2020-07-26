(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let char_to_hex c =
  Printf.sprintf "0x%02x" (Char.code c)

let int_to_hex n =
  Printf.sprintf "0x%x" n

type error =
  | Truncated_file
  | Unrecognized of string
  | Unsupported of string * int64
  | Out_of_range of string

let error_to_string = function
  | Truncated_file ->
      "Truncated file"
  | Unrecognized magic ->
      Printf.sprintf "Unrecognized magic: %s"
        (String.concat " "
           (List.init (String.length magic)
              (fun i -> char_to_hex magic.[i])))
  | Unsupported (s, n) ->
      Printf.sprintf "Unsupported: %s: 0x%Lx" s n
  | Out_of_range s ->
      Printf.sprintf "Out of range constant: %s" s

exception Error of error

let name_at ?max_len buf start =
  if start < 0 || start > Bytes.length buf then
    raise (Error (Out_of_range (int_to_hex start)));
  let max_pos =
    match max_len with
    | None -> Bytes.length buf
    | Some n -> min (Bytes.length buf) (start + n)
  in
  let rec loop pos =
    if pos >= max_pos || Bytes.get buf pos = '\000'
    then
      Bytes.sub_string buf start (pos - start)
    else
      loop (succ pos)
  in
  loop start

let really_input_bytes ic len =
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  buf

let uint64_of_uint32 n =
  Int64.(logand (of_int32 n) 0xffffffffL)

type endianness =
  | LE
  | BE

type bitness =
  | B32
  | B64

type decoder =
  {
    ic: in_channel;
    endianness: endianness;
    bitness: bitness;
  }

let word_size = function
  | {bitness = B64; _} -> 8
  | {bitness = B32; _} -> 4

let get_uint16 {endianness; _} buf idx =
  match endianness with
  | LE -> Bytes.get_uint16_le buf idx
  | BE -> Bytes.get_uint16_be buf idx

let get_uint32 {endianness; _} buf idx =
  match endianness with
  | LE -> Bytes.get_int32_le buf idx
  | BE -> Bytes.get_int32_be buf idx

let get_uint s d buf idx =
  let n = get_uint32 d buf idx in
  match Int32.unsigned_to_int n with
  | None -> raise (Error (Unsupported (s, Int64.of_int32 n)))
  | Some n -> n

let get_uint64 {endianness; _} buf idx =
  match endianness with
  | LE -> Bytes.get_int64_le buf idx
  | BE -> Bytes.get_int64_be buf idx

let get_word d buf idx =
  match d.bitness with
  | B64 -> get_uint64 d buf idx
  | B32 -> uint64_of_uint32 (get_uint32 d buf idx)

let uint64_to_int s n =
  match Int64.unsigned_to_int n with
  | None -> raise (Error (Unsupported (s, n)))
  | Some n -> n

let load_bytes d off len =
  LargeFile.seek_in d.ic off;
  really_input_bytes d.ic len

(* Reference: http://man7.org/linux/man-pages/man5/elf.5.html *)

let header_size d =
  40 + 3 * word_size d

type header =
  {
    e_shoff: int64;
    e_shentsize: int;
    e_shnum: int;
    e_shstrndx: int;
  }

let read_header d =
  let buf = load_bytes d 0L (header_size d) in
  let word_size = word_size d in
  let e_shnum = get_uint16 d buf (36 + 3 * word_size) in
  let e_shentsize = get_uint16 d buf (34 + 3 * word_size) in
  let e_shoff = get_word d buf (24 + 2 * word_size) in
  let e_shstrndx = get_uint16 d buf (38 + 3 * word_size) in
  {e_shnum; e_shentsize; e_shoff; e_shstrndx}

type sh_type =
  | SHT_STRTAB
  | SHT_DYNSYM
  | SHT_OTHER

type section =
  {
    sh_name: int;
    sh_type: sh_type;
    sh_addr: int64;
    sh_offset: int64;
    sh_size: int;
    sh_entsize: int;
    sh_name_str: string;
  }

let load_section_body d {sh_offset; sh_size; _} =
  load_bytes d sh_offset sh_size

let read_sections d {e_shoff; e_shnum; e_shentsize; e_shstrndx; _} =
  let buf = load_bytes d e_shoff (e_shnum * e_shentsize) in
  let word_size = word_size d in
  let mk i =
    let base = i * e_shentsize in
    let sh_name = get_uint "sh_name" d buf (base + 0) in
    let sh_type =
      match get_uint32 d buf (base + 4) with
      | 3l -> SHT_STRTAB
      | 11l -> SHT_DYNSYM
      | _ -> SHT_OTHER
    in
    let sh_addr = get_word d buf (base + 8 + word_size) in
    let sh_offset = get_word d buf (base + 8 + 2 * word_size) in
    let sh_size =
      uint64_to_int "sh_size"
        (get_word d buf (base + 8 + 3 * word_size))
    in
    let sh_entsize =
      uint64_to_int "sh_entsize"
        (get_word d buf (base + 16 + 5 * word_size))
    in
    {sh_name; sh_type; sh_addr; sh_offset;
     sh_size; sh_entsize; sh_name_str = ""}
  in
  let sections = Array.init e_shnum mk in
  if e_shstrndx = 0 then
    (* no string table *)
    sections
  else
    let shstrtbl = load_section_body d sections.(e_shstrndx) in
    let set_name sec =
      let sh_name_str = name_at shstrtbl sec.sh_name in
      {sec with sh_name_str}
    in
    Array.map set_name sections

let read_sections d h =
  let {e_shoff; e_shentsize; e_shnum; e_shstrndx} = h in
  if e_shoff = 0L then
    [||]
  else begin
    let buf = lazy (load_bytes d e_shoff e_shentsize) in
    let word_size = word_size d in
    let e_shnum =
      if e_shnum = 0 then
        (* The real e_shnum is the sh_size of the initial section.*)
        uint64_to_int "e_shnum"
          (get_word d (Lazy.force buf) (8 + 3 * word_size))
      else
        e_shnum
    in
    let e_shstrndx =
      if e_shstrndx = 0xffff then
        (* The real e_shstrndx is the sh_link of the initial section. *)
        get_uint "e_shstrndx" d (Lazy.force buf) (8 + 4 * word_size)
      else
        e_shstrndx
    in
    read_sections d {h with e_shnum; e_shstrndx}
  end
