(*****************************************************************************

  SoapLab, an integrated automation system.
  Copyright 2007 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open GtkHelper

exception No_selection

let get_some x =
  match x with
    | Some x -> x
    | None -> assert false

class sound_editor ?(file="ias.glade") =
object (self)
  inherit Ias.sound_editor ~file () as super

  val mutable sample_rate = 44100

  (* Play *)
  val mutable playing = false
  val mutable play_thread = None
  (* Zoom *)
  val zoom_step = 1.2
  val mutable zoom_thread = None

  (* Progress bar stuff *)
  val mutable wd = None

  (* Tempo *)
  val mutable tempo = 120.
  val mutable tempo_offset = 0
  val mutable tempo_color = `RGB (0, 0, max_int / 6)
  val mutable tempo_beat_width = 22050
  val mutable tempo_stretcher_color = `RGB (max_int / 5, max_int / 5, max_int / 5)

  initializer
    ignore (self#toplevel#connect#destroy ~callback:self#on_destroy);

    (* Connect them *)
    ignore (tbtn_play#connect#clicked ~callback:self#on_play);
    ignore (tbtn_stop#connect#clicked ~callback:self#on_stop);
    ignore (tbtn_loop#connect#clicked ~callback:(fun () -> self#on_play ~loop:true ()));
    ignore (tbtn_import#connect#clicked ~callback:self#on_open);
    ignore (tbtn_export#connect#clicked ~callback:self#on_save);
    ignore (tbtn_zoom_fit#connect#clicked ~callback:(fun () -> hsb#adjustment#set_value 0.; self#set_zoom (get_some wd)#get_zoom_fit));
    ignore (tbtn_zoom_sel#connect#clicked ~callback:(fun () -> self#set_zoom (get_some wd)#get_zoom_selection; hsb#adjustment#set_value (float_of_int (get_some wd)#selection_start)));
    ignore (tbtn_zoom_100#connect#clicked ~callback:(fun () -> self#set_zoom (get_some wd)#get_zoom_one));
    ignore (tbtn_zoom_in#connect#clicked  ~callback:(fun () -> self#set_zoom ((get_some wd)#zoom *. zoom_step)));
    ignore (tbtn_zoom_out#connect#clicked ~callback:(fun () -> self#set_zoom ((get_some wd)#zoom /. zoom_step)));
    ignore (spin_tempo#connect#value_changed ~callback:(fun () -> self#set_tempo spin_tempo#value));
    ignore (spin_tempo_offset#connect#value_changed ~callback:(fun () -> tempo_offset <- spin_tempo_offset#value_as_int; self#draw_tempo_markers));

    (* File menu *)
    ignore (self#menu_open#connect#activate ~callback:self#on_open);
    ignore (self#menu_save#connect#activate ~callback:self#on_save);
    (* Edit menu *)
    ignore (self#menu_copy#connect#activate ~callback:self#on_copy);
    ignore (self#menu_paste#connect#activate ~callback:self#on_paste);
    ignore (self#menu_cut#connect#activate ~callback:(self#on_cut ~delete:false));
    ignore (self#menu_delete#connect#activate ~callback:(self#on_cut ~delete:true));
    (* Tools menu *)
    ignore (self#menu_normalize#connect#activate ~callback:self#on_normalize);
    ignore (self#menu_cut_blanks#connect#activate ~callback:self#on_cut_blanks);
    ignore (self#menu_rms#connect#activate ~callback:self#on_rms);
    (* View menu *)
    ignore (menu_zoom_fit#connect#activate ~callback:(fun () -> hsb#adjustment#set_value 0.; self#set_zoom (get_some wd)#get_zoom_fit));
    ignore (menu_zoom_selection#connect#activate ~callback:(fun () -> self#set_zoom (get_some wd)#get_zoom_selection; hsb#adjustment#set_value (float_of_int (get_some wd)#selection_start)));
    ignore (self#menu_zoom_normal#connect#activate ~callback:(fun () -> self#set_zoom (get_some wd)#get_zoom_one));
    ignore (self#menu_zoom_in#connect#activate ~callback:(fun () -> self#set_zoom ((get_some wd)#zoom *. zoom_step)));
    ignore (self#menu_zoom_out#connect#activate ~callback:(fun () -> self#set_zoom ((get_some wd)#zoom /. zoom_step)));
    (* Help menu *)
    ignore (self#menu_about#connect#activate ~callback:on_about);

    (* Zoom *)
    zsb#adjustment#set_bounds ~lower:0.001 ~upper:3000. ~step_incr:10. ();
    ignore (zsb#adjustment#connect#value_changed ~callback:self#on_zoom);

    (* GtkWaveDisplay *)
    ignore (wave_display#event#connect#button_press ~callback:self#on_da_button_press);
    ignore (wave_display#event#connect#button_release ~callback:self#on_da_button_release);

    (* It's showtime, folks! *)
    toplevel#show ();
    progress#misc#hide ();

    (* Window must already be shown for wave_display to be called. *)
    wd <- Some (GtkWaveDisplay.wave_display ~progress:progress hsb wave_display) ;

    (get_some wd)#disable_smart_zoom;
    zsb#set_update_policy `CONTINUOUS

  method draw_tempo_markers =
    let wd = get_some wd in
    let markers = wd#markers in
    let markers = List.filter (fun (_, c) -> c <> tempo_color) markers in
    wd#set_markers markers;
    for i = 0 to (wd#length - tempo_offset) / tempo_beat_width do
      wd#add_marker (tempo_offset + i * tempo_beat_width) tempo_color
    done

  method set_tempo t =
    let adj = spin_tempo_offset#adjustment in
    tempo <- t;
    tempo_beat_width <- int_of_float (float sample_rate /. (tempo /. 60.));
    adj#set_bounds ~upper:(float tempo_beat_width) ();
    spin_tempo_offset#set_adjustment adj;
    self#draw_tempo_markers

  (* Play a sound *)
  method on_play ?(loop=false) () =
    playing <- false;
    while play_thread <> None do
      Thread.yield () (* TODO: find something better *)
    done;
    play_thread <-
      Some (
          Thread.create
            (fun () ->
              try
                let old_cursor = (get_some wd)#cursor in
                let chans = (get_some wd)#channels in
                (* TODO: open the stream once for all *)
                let dev = Portaudio.open_default_stream 0 chans sample_rate 256 in
                let buflen = 1024 in
                let buf = Array.init chans (fun _ -> Array.make buflen 0.) in
                let len = ref (-1) in
                if loop then
                  (get_some wd)#set_cursor (get_some wd)#selection_start;
                Portaudio.start_stream dev;
                playing <- true;
                while playing && (!len <> 0 || loop) && (not loop || (get_some wd)#selection_length <> 0) do
                  if !len = 0 && loop then
                    (get_some wd)#set_cursor (get_some wd)#selection_start;
                  let ofs = (get_some wd)#cursor in
                  let buflen =
                    if loop then
                      min buflen ((get_some wd)#selection_start + (get_some wd)#selection_length - ofs)
                    else
                      buflen
                  in
                  len := (get_some wd)#get_data ofs buf 0 buflen;
                  (get_some wd)#set_cursor (ofs + !len);
                  entry_position#set_text (Tools.seconds_to_minsec (float_of_int ofs /. (get_some wd)#samples_per_second)) ;
                  Portaudio.write_stream dev buf 0 !len
                done;
                (get_some wd)#set_cursor old_cursor;
                Portaudio.stop_stream dev;
                Portaudio.close_stream dev;
                playing <- false;
                play_thread <- None
              with
              | Portaudio.Error n ->
                 play_thread <- None;
                 error_dialog ~parent:toplevel ~title:"Portaudio error" ((Portaudio.string_of_error n) ^ ".")
            ) ()
        )

  method on_stop () =
    playing <-false

  (* Zoom *)
  method zoom_progress f () =
    (*
     match !zoom_thread with
     | Some t -> Thread.kill t
     | None -> ()
     *)
    zoom_thread <-
      Some
        (
          Thread.create
            (fun () ->
              progress#set_text "Computing zoomed waveform...";
              progress#set_fraction 0.;
              progress#misc#show ();
              f ();
              progress#misc#hide ();
              zoom_thread <- None
            ) ()
        )

  method on_zoom () =
    self#zoom_progress (fun () -> (get_some wd)#set_zoom zsb#adjustment#value) ()

  method set_zoom n =
    zsb#adjustment#set_value n

  method big_chunks ~chunk_size ~channels =
    let total  = ref 0 in
    let maxj   = ref 0 in
    let chunks = ref [] in
    let feed i j v =
      while j >= !total do
        let ba = Tools.alloc_buffer channels chunk_size in
        total := !total + chunk_size ;
        chunks := ba :: !chunks
      done ;
      let rec insert total chunks =
        match chunks with
        | hd::tl ->
           let total = total - chunk_size in
           if j >= total then
             Bigarray.Array2.set hd (j - total) i v
           else
             insert total tl
        | [] -> assert false
      in
      maxj := max !maxj j ;
      insert !total !chunks
    in
    let finish () =
      let real_len = 1 + !maxj in
      let ba = Tools.alloc_buffer channels real_len in
      ignore
        (List.fold_left
           (fun (off,len) subarray ->
             let subsub = Bigarray.Array2.sub_left subarray 0 len in
             let suball = Bigarray.Array2.sub_left ba off len in
             Bigarray.Array2.blit subsub suball ;
             (off-chunk_size,chunk_size))
           (!total-chunk_size,
            let l = real_len mod chunk_size in
            if l = 0 then chunk_size else l)
           !chunks) ;
      ba
    in
    feed,finish

  (* Open a sound file *)
  method open_file f =
    let open_file f =
      progress#set_fraction 0. ;
      progress#set_text "Decoding...";
      progress#misc#show ();
      let dec, fd = Vorbis.File.Decoder.openfile f in
      let chans = 2 in
      let totlen = Vorbis.File.Decoder.samples dec (Vorbis.File.Decoder.bitstream dec) in
      Printf.printf "%d samples\n%!" totlen;
      let bigbuf = Tools.alloc_buffer 2 totlen in
      let buflen = 1024 in
      let buf = Array.init chans (fun _ -> Array.make buflen 0.) in
      let off = ref 0 in
      Printf.printf "Reading data...\n%!" ;
      while !off < totlen do
        let len = Vorbis.File.Decoder.decode_float dec buf 0 buflen in
        for c = 0 to chans - 1 do
          let buf_c = buf.(c) in
          for i = 0 to len - 1 do
            Bigarray.Array2.set bigbuf (!off + i) c buf_c.(i)
          done
        done;
        off := !off + len;
        progress#set_fraction ((float !off) /. (float totlen))
      done;
      Printf.printf "Track done\n%!";
      progress#set_text "Decoded.";
      Unix.close fd;
      (get_some wd)#set_data bigbuf;
      self#set_zoom (get_some wd)#get_zoom_fit;
      self#draw_tempo_markers
    in
    ignore (Thread.create open_file f)

  (* Open callback *)
  method on_open () =
    let fcd =
      GWindow.file_chooser_dialog
        ~action:`OPEN
        ~title:"Please choose a file"
        ()
    in
    fcd#add_button_stock `CANCEL `CANCEL;
    fcd#add_select_button_stock `OPEN `ACCEPT;
    if fcd#run () = `ACCEPT then
      (
        match fcd#filename with
        | Some f -> self#open_file f
        | None -> ()
      );
    fcd#destroy ()

  method save_file ofs len f =
    Tools.ignore_thread
      (fun () ->
        progress#set_fraction 0. ;
        progress#set_text "Encoding...";
        progress#misc#show ();
        let fd = Unix.openfile f [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
        let write s = ignore (Unix.write fd (Bytes.of_string s) 0 (String.length s)) in
        let write_page (ph,pb) = write (ph^pb) in
        let enc = Vorbis.Encoder.create_vbr (get_some wd)#channels (int_of_float (get_some wd)#samples_per_second) 0.2 in
        let os = Ogg.Stream.create () in
        let pos = ref 0 in
        Vorbis.Encoder.headerout enc os [];
        write_page (Ogg.Stream.flush_page os);
        (get_some wd)#iter ofs len
          (fun buf ->
            Vorbis.Encoder.encode_buffer_float enc os buf 0 (Array.length buf.(0));
            (
              try
                while true do
                  write_page (Ogg.Stream.get_page os)
                done
              with
              | Ogg.Not_enough_data -> ()
            );
            pos := !pos + Array.length buf.(0);
            progress#set_fraction (float_of_int !pos /. float_of_int len)
          );
        Vorbis.Encoder.end_of_stream enc os;
        write_page (Ogg.Stream.flush_page os);
        Unix.close fd;
        progress#misc#hide ()
      )

  (* Save callback *)
  method on_save () =
    let fcd =
      GWindow.file_chooser_dialog
        ~action:`SAVE
        ~title:"Please choose a file"
        ()
    in
    fcd#add_button_stock `CANCEL `CANCEL;
    fcd#add_select_button_stock `SAVE `ACCEPT;
    if fcd#run () = `ACCEPT then
      (
        match fcd#filename with
        | Some f ->
           self#save_file 0 (get_some wd)#length f
        | None -> ()
      );
    fcd#destroy ()

  (* Handle marker positionning on click *)
  method on_da_button_press be =
    let wd = get_some wd in
    entry_position#set_text (Tools.seconds_to_minsec (float_of_int wd#cursor /. wd#samples_per_second));
    entry_selection_start#set_text (Tools.seconds_to_minsec (float_of_int wd#selection_start /. wd#samples_per_second));
    false

  val mutable stretchers = []

  method on_da_button_release be =
    let wd = get_some wd in
    let state = Gdk.Convert.modifier (GdkEvent.Button.state be) in
    entry_selection_start#set_text (Tools.seconds_to_minsec (float_of_int wd#selection_start /. wd#samples_per_second));
    entry_selection_end#set_text (Tools.seconds_to_minsec (float_of_int (wd#selection_start + wd#selection_length) /. wd#samples_per_second));
    if List.mem `CONTROL state then
      (
        let x = GdkEvent.Button.x be in
        let x = wd#sample_of_x x in
        let markers = List.filter (fun (_, c) -> c <> tempo_stretcher_color) wd#markers in
        let nearest_stretcher =
          let ans = ref 0 in
          let d_ans = ref max_int in
          List.iter
            (fun (n, _) ->
              let d = abs (n - x) in
              if d < !d_ans then
                (
                  ans := n;
                  d_ans := d
                )
            ) stretchers
        in
        (* wd#set_markers markers; *)
        (
          match GdkEvent.Button.button be with
          | 3 ->
             wd#add_marker x tempo_stretcher_color
          | 1 -> ()
          | _ -> ()
        );
      );
    false

  (* Normalize *)
  method on_normalize () =
    let m = Array.make (get_some wd)#channels 0. in
    (get_some wd)#iter_selection
      (fun buf ->
        for c = 0 to Array.length buf - 1 do
          let bufc = buf.(c) in
          for i = 0 to Array.length bufc - 1 do
            m.(c) <- max m.(c) (abs_float bufc.(i))
          done;
        done
      );
    let m = Array.fold_left min 0. m in
    (get_some wd)#map_selection
      (fun buf ->
        for c = 0 to Array.length buf - 1 do
          let bufc = buf.(c) in
          for i = 0 to Array.length bufc - 1 do
            bufc.(i) <- bufc.(i) /. m
          done;
        done;
        buf
      )

  (* Compute RMS *)
  method on_rms () =
    let rms = Array.make (get_some wd)#channels 0. in
    (get_some wd)#iter_selection
      (fun buf ->
        for c = 0 to Array.length buf - 1 do
          let bufc = buf.(c) in
          for i = 0 to Array.length bufc - 1 do
            rms.(c) <- rms.(c) +. bufc.(i) *. bufc.(i)
          done;
        done
      );
    let rms =
      Array.map (fun r -> sqrt (r /. float_of_int (get_some wd)#selection_length)) rms
    in
    let rms =
      Array.map (fun r -> 20. *. log r /. log 10.) rms
    in
    let s =
      let s = ref "" in
      for c = 0 to Array.length rms -  1 do
        s := Printf.sprintf "%schannel %d: %5.02f dB\n" !s c rms.(c)
      done;
      !s
    in
    let dialog =
      GWindow.message_dialog
        ~title:"RMS"
        ~parent:toplevel ~message_type:`INFO ~buttons:GWindow.Buttons.ok
        ~message:s ()
    in
    ignore (dialog#run ());
    dialog#destroy ()

  val mutable clipboard = Tools.alloc_buffer 2 1

  method on_copy () =
    let wd = get_some wd in
    clipboard <- Tools.alloc_buffer wd#channels wd#selection_length;
    Tools.buffer_blit wd#data wd#selection_start clipboard 0 wd#selection_length

  method delete_sound start len =
    let wd = get_some wd in
    let new_data = Tools.alloc_buffer wd#channels (wd#length - len) in
    Tools.buffer_blit wd#data 0 new_data 0 start;
    Tools.buffer_blit
      wd#data (start + len)
      new_data start
      (wd#length - (start + len));
    wd#set_data ~freq:(wd#samples_per_second) new_data;
    wd#expose

  method on_cut ~delete () =
    if not delete then self#on_copy ();
    let wd = get_some wd in
    wd#set_selection wd#selection_start 0;
    self#delete_sound wd#selection_start wd#selection_length

  method on_paste () =
    let wd = get_some wd in
    let clip_len = Bigarray.Array2.dim1 clipboard in
    let new_data = Tools.alloc_buffer wd#channels (wd#length + clip_len) in
    Tools.buffer_blit wd#data 0 new_data 0 wd#cursor;
    Tools.buffer_blit clipboard 0 new_data wd#cursor clip_len;
    Tools.buffer_blit wd#data wd#cursor new_data (wd#cursor + clip_len) (wd#length - wd#cursor);
    wd#set_data ~freq:(wd#samples_per_second) new_data;
    wd#set_selection wd#cursor clip_len;
    wd#expose

  method on_cut_blanks () =
    let wd = get_some wd in
    let is_blank v = abs_float v < 0.01 in
    let is_blank s = Array.fold_left (fun b v -> b && is_blank v) true s in
    let is_blank n = is_blank (wd#get n) in
    let blank_start = ref 0 in
    let blank_end = ref (wd#length - 1) in
    while is_blank !blank_start && !blank_start < wd#length do
      incr blank_start
    done;
    while is_blank !blank_end && !blank_end >= 0 do
      decr blank_end
    done;
    self#delete_sound 0 !blank_start;
    if !blank_end < wd#length - 1 then
      self#delete_sound (!blank_end + 1) (wd#length - (!blank_end + 1))

  method on_destroy () =
    Tools.unlink_temp_files ()
end
