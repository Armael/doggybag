open Bos
let (let*) = Result.bind

let rec riter l f = match l with
  | [] -> Ok ()
  | x :: xs ->
    let* () = f x in
    riter xs f

let copy ?mode src dst =
  begin
    let* mode = match mode with
      | None -> OS.Path.Mode.get src
      | Some m -> Ok m
    in
    let* contents = OS.File.read src in
    OS.File.write ~mode dst contents
  end
  |> Result.map_error (fun (`Msg msg) ->
    `Msg (Printf.sprintf "could not copy %s to %s: %s"
            (Fpath.to_string src) (Fpath.to_string dst) msg)
  )

let dynamic_dependencies exe =
  let* ldd_lines =
    OS.Cmd.success @@ OS.Cmd.out_lines @@
    OS.Cmd.run_out Cmd.(v "ldd" % p exe)
  in
  Result.ok @@
  List.filter_map (fun line ->
    let elts =
      String.split_on_char ' ' (String.trim line)
      |> List.filter ((<>) "") in
    match elts with
    | [so_name; "=>"; fullpath; _] ->
      (* XXX: is this reliable? *)
      if so_name = Filename.basename fullpath then
        Some (Fpath.v fullpath)
      else
        None
    | _ ->
      None
  ) ldd_lines

let desktop_file ~icon ~name ~exec =
  Printf.sprintf {|[Desktop Entry]
Categories=Utility;
Type=Application
Icon=%s
Name=%s
Exec=%s
|} icon name exec

let main () =
  let src, dst =
    match Sys.argv |> Array.to_list |> List.tl with
    | src :: dst :: [] -> src, dst
    | _ ->
      Printf.eprintf "usage: %s <binary> <output-binary>\n" Sys.argv.(0);
      exit 1
  in

  let* src = OS.File.must_exist (Fpath.v src) in
  (* create the appdir directory *)

  OS.Dir.with_tmp "doggybag_%s" (fun tmpdir () ->

    let* _ = OS.Dir.create ~path:true Fpath.(tmpdir / "usr" / "bin") in
    let* _ = OS.Dir.create ~path:true Fpath.(tmpdir / "usr" / "lib") in

    (* Copy the binary *)
    let* () = copy ~mode:0o755 src Fpath.(tmpdir / "usr" / "bin" / "exe") in

    (* run ldd on the binary, parse its output to get the dependencies *)
    let* dyndeps = dynamic_dependencies src in

    (* copy the .so dependencies in the appdir directory *)
    let* () =
      riter dyndeps (fun dep ->
        copy ~mode:0o755 dep Fpath.(tmpdir / "usr" / "lib" / basename dep)
      )
    in

    (* use patchelf to set the rpath for the .so, and set a filler rpath for the
       binary itself *)
    let* () =
      riter dyndeps (fun dep ->
        OS.Cmd.success @@ OS.Cmd.out_null @@ OS.Cmd.run_out @@
        Cmd.(v "patchelf"
             % "--set-rpath" % "$ORIGIN"
             % "--force-rpath"
             % p Fpath.(tmpdir / "usr" / "lib" / basename dep))
      ) in

    let exe_rpath_filler = String.make 256 'Z' in
    let* () =
      OS.Cmd.success @@ OS.Cmd.out_null @@ OS.Cmd.run_out @@
      Cmd.(v "patchelf"
           % "--set-rpath" % exe_rpath_filler
           % "--force-rpath"
           % p Fpath.(tmpdir / "usr" / "bin" / "exe")) in

    (* get interpreter using patchelf *)
    let* ld_so =
      OS.Cmd.success @@ OS.Cmd.out_string @@ OS.Cmd.run_out @@
      Cmd.(v "patchelf" % "--print-interpreter" % Fpath.to_string src) in

    (* copy the dynamic libraries loader in the image *)
    let ld_so = Fpath.v ld_so in
    let* () = copy ~mode:0o755 ld_so Fpath.(tmpdir / "usr" / "lib" / "ld.so") in

    (* use patchelf to set interpreter with ld.so filler *)
    (* patchelf already uses 'X's to write over patched sections, so use an
       other character *)
    let interp_filler = String.make 2048 'W' in
    let* () =
      OS.Cmd.success @@ OS.Cmd.out_null @@ OS.Cmd.run_out @@
      Cmd.(v "patchelf"
           % "--set-interpreter" % interp_filler
           % p Fpath.(tmpdir / "usr" / "bin" / "exe")) in

    (* copy in AppRun *)
    let* () =
      OS.File.write ~mode:0o755 Fpath.(tmpdir / "AppRun")
        Doggybag_apprun_embed.data in

    (* myapp = basename of dst *)
    let myapp = Filename.basename dst |> Filename.remove_extension |> String.lowercase_ascii in
    let png = myapp ^ ".png" in

    (* copy in the dummy .png for myapp.png *)
    let* () = OS.File.write Fpath.(tmpdir / png) Doggybag_icon_embed.data in

    (* link myapp.png to .DirIcon *)
    let* () = OS.Path.symlink ~target:Fpath.(tmpdir / png) Fpath.(tmpdir / ".DirIcon") in

    (* copy in a dummy .desktop *)
    let* () =
      OS.File.write Fpath.(tmpdir / (myapp ^ ".desktop"))
        (desktop_file ~icon:myapp ~name:myapp ~exec:myapp) in

    (* run the appimagetool on the tmpdir to produce dst *)
    OS.Cmd.success @@ OS.Cmd.out_stdout @@ OS.Cmd.run_out @@
    Cmd.(v "appimagetool" % p tmpdir % dst)
  ) ()
  |> Result.join

let () =
  match main () with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
