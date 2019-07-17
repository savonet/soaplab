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

(* let clipboard = GData.clipboard Gdk.Atom.clipboard *)

let usage = Sys.argv.(0) ^ " [--editor <file>]"
let file = ref None

let () =
  Printexc.record_backtrace true;
  Arg.parse [] (fun s -> file := Some s) usage

let se =
  new SoundEditor.sound_editor ~file:(Filename.dirname Sys.argv.(0) ^ "/ias.glade")

let () =
  (* Connect quit *)
  ignore (se#toplevel#connect#destroy ~callback:on_quit) ;
  ignore (se#menu_quit#connect#activate ~callback:on_quit) ;

  (* File *)
  Option.iter se#open_file !file;

  (* Start the event loop. *)
  GtkThread.main ()
