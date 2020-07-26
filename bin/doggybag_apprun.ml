(* Relative paths from the root of the squashfs image *)
(* the program binary *)
let binary = "usr/bin/exe"
(* the path to the dynamic libraries loader, from which we derive the full path
   to patch in the binary *)
let ld_so = "usr/lib/ld.so"

let () =
  (* Read the APPDIR environment variable set by the appimage loader. It
     indicates the mountpoint of the squashfs image. *)
  let mountpoint =
    match Sys.getenv_opt "APPDIR" with
    | Some mountpoint -> mountpoint
    | None -> failwith "APPDIR not set. Were we executed by the appimage loader?"
  in

  (* From this, get the full paths for the binary and ld.so *)
  let binary = mountpoint ^ "/" ^ binary in
  let ld_so = mountpoint ^ "/" ^ ld_so in
  let runpath = mountpoint ^ "/usr/lib" in

  let patched_fd = Libhack.with_open_in binary (fun ic ->
    let dec = Libelf.Elf.{ ic; endianness = LE; bitness = B64 } in
    let hdr = Libelf.Elf.read_header dec in
    let sections = Libelf.Elf.read_sections dec hdr in

    (* Read the ELF sections index of the binary using Libelf, find the offset and
       size of the .interp section, and compute patches for RPATH/RUNPATH *)
    let interp_loc = Libelf.Patch.find_section_loc ".interp" sections in
    let dynamic_loc = Libelf.Patch.find_section_loc ".dynamic" sections in
    let dynstr_loc = Libelf.Patch.find_section_loc ".dynstr" sections in

    let rpath_patches = ref [] in
    Libelf.Patch.iter_dyntags dec dynamic_loc (fun ~dtag ~dval ->
      if dtag = 29L (* DT_RUNPATH *) || dtag = 15L (* DT_RPATH *) then begin
        let val_offset = dynstr_loc.offset + Int64.to_int dval in
        let val_size = Libelf.Patch.string_length dec val_offset + 1 (* \0 *) in
        rpath_patches :=
          Libelf.Patch.{
            loc = { offset = val_offset; size = val_size };
            patch = runpath
          } :: !rpath_patches
      end
    );

    let patches =
      Libelf.Patch.{ loc = interp_loc; patch = ld_so } ::
      !rpath_patches
    in

    (* Use memfd_create to obtain a memory-backed file descriptor for the
       patched binary *)
    let patched_fd = Libhack.memfd_create () in

    (* Copy the binary to the patched binary fd, with a modified .interp section
       and patched RPATH/RUNPATH *)
    let oc = Libhack.outchannel_of_file_descr patched_fd in
    Libelf.Patch.copy_and_patch patches ic oc;

    patched_fd
  ) in

  (* Run the patched binary from memory *)
  Libhack.execv_memfd patched_fd Sys.argv
