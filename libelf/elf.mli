val char_to_hex : char -> string
val int_to_hex : int -> string
type error =
    Truncated_file
  | Unrecognized of string
  | Unsupported of string * int64
  | Out_of_range of string
val error_to_string : error -> string
exception Error of error
val name_at : ?max_len:int -> bytes -> int -> string
val really_input_bytes : in_channel -> int -> bytes
val uint64_of_uint32 : int32 -> int64
type endianness = LE | BE
type bitness = B32 | B64
type decoder = {
  ic : in_channel;
  endianness : endianness;
  bitness : bitness;
}
val word_size : decoder -> int
val get_uint16 : decoder -> bytes -> int -> int
val get_uint32 : decoder -> bytes -> int -> int32
val get_uint : string -> decoder -> bytes -> int -> int
val get_uint64 : decoder -> bytes -> int -> int64
val get_word : decoder -> bytes -> int -> int64
val uint64_to_int : string -> int64 -> int
val load_bytes : decoder -> int64 -> int -> bytes
val header_size : decoder -> int
type header = {
  e_shoff : int64;
  e_shentsize : int;
  e_shnum : int;
  e_shstrndx : int;
}
val read_header : decoder -> header
type sh_type = SHT_STRTAB | SHT_DYNSYM | SHT_OTHER
type section = {
  sh_name : int;
  sh_type : sh_type;
  sh_addr : int64;
  sh_offset : int64;
  sh_size : int;
  sh_entsize : int;
  sh_name_str : string;
}
val load_section_body : decoder -> section -> bytes
val read_sections : decoder -> header -> section array
