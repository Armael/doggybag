type loc = { offset: int; size: int }

let find_section_loc (section_name: string) sections : loc =
  let offset = ref (-1L) in
  let size = ref (-1) in
  Array.iter (fun s ->
    if s.Elf.sh_name_str = section_name then (
      offset := s.Elf.sh_offset;
      size := s.Elf.sh_size;
    )
  ) sections;
  if !offset < 0L then
    failwith (section_name ^ ": section not found, or invalid offset");
  { offset = Int64.to_int !offset; size = !size }

let read_section_loc (section_name: string) (dec: Elf.decoder) : loc =
  let hdr = Elf.read_header dec in
  let sections = Elf.read_sections dec hdr in
  find_section_loc section_name sections

let iter_dyntags (dec: Elf.decoder) (loc: loc) f =
  let ws = Elf.word_size dec in
  let buf = Bytes.create (2 * ws) in
  seek_in dec.ic loc.offset;
  let rec loop pos =
    if pos < loc.offset + loc.size then (
      really_input dec.ic buf 0 (2 * ws);
      let dtag = Elf.get_word dec buf 0 in
      let dval = Elf.get_word dec buf ws in
      f ~dtag ~dval;
      let pos = pos + 2 * ws in
      seek_in dec.ic pos; (* in case [f] moved around in the file *)
      loop pos
    )
  in
  loop loc.offset

let string_length (dec: Elf.decoder) (offset: int) =
  seek_in dec.ic offset;
  let rec loop len =
    if input_byte dec.ic = 0 then len
    else loop (len + 1)
  in
  loop 0

type patch = { loc: loc; patch: string }

let copy_and_patch (patches: patch list) (ic: in_channel) (oc: out_channel) =
  List.iter (fun { loc; patch } ->
    if loc.size < String.length patch + 1 (* final \0 *) then
      failwith ("Unable to apply patch " ^ patch ^ ": not enough space")
  ) patches;

  let patches = List.sort (fun p1 p2 ->
    let offset { loc = { offset; _ }; _ } = offset in
    compare (offset p1) (offset p2)
  ) patches in

  (* sanity check: patches must not overlap *)
  let rec nonoverlap = function
    | [] | [_] -> true
    | p1 :: p2 :: ps ->
      (p1.loc.offset + p1.loc.size <= p2.loc.offset) &&
      nonoverlap (p2 :: ps)
  in

  if not (nonoverlap patches) then
    raise (Invalid_argument "copy_and_patch: overlapping patches");

  let buf = Bytes.create 2048 in

  let output_zeroes (oc: out_channel) (nb: int) =
    Bytes.fill buf 0 (min 2048 nb) (Char.chr 0);
    let rec loop remaining =
      if remaining > 0 then
        let n = min 2048 remaining in
        output oc buf 0 n;
        loop (remaining - n)
    in
    loop nb
  in

  let rec loop patches pos =
    match patches with
    | [] ->
      let nb_bytes_read = input ic buf 0 2048 in
      if nb_bytes_read = 0 then (* End_of_file *)
        ()
      else (
        output oc buf 0 nb_bytes_read;
        loop [] (pos + nb_bytes_read)
      )
    | { loc; patch } :: patches' ->
      if pos < loc.offset then (
        let nb_bytes_read = min 2048 (loc.offset - pos) in
        really_input ic buf 0 nb_bytes_read;
        output oc buf 0 nb_bytes_read;
        loop patches (pos + nb_bytes_read)
      ) else (
        assert (pos = loc.offset);
        output_string oc patch;
        output_zeroes oc (loc.size - String.length patch);
        seek_in ic (loc.offset + loc.size);
        loop patches' (loc.offset + loc.size)
      )
  in

  seek_in ic 0;
  loop patches 0;
  flush oc
