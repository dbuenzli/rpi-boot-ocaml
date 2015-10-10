(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rpi

let activity_led = Gpio.P47

let draw_image fb (w, h, s) =
  let start_x, start_y = (Fb.w fb - w) / 2, (Fb.h fb - h) / 2 in
  let x_stride, y_stride = 4, Fb.stride fb in
  let buf = Fb.buffer fb in
  for y = 0 to h - 1  do
    let k = (start_y + y) * y_stride in
    let i = y * (w * 3) in
    for x = 0 to w - 1 do
      let k = k + (start_x + x) * x_stride in
      let i = i + x * 3 in
      let b = Char.code (String.unsafe_get s (i    )) in
      let g = Char.code (String.unsafe_get s (i + 1)) in
      let r = Char.code (String.unsafe_get s (i + 2)) in
      Bigarray.Array1.unsafe_set buf (k    ) b;
      Bigarray.Array1.unsafe_set buf (k + 1) g;
      Bigarray.Array1.unsafe_set buf (k + 2) r;
    done
  done

let draw_blink fb blink =
  let x, y = Fb.w fb / 2 + 67, Fb.h fb / 2 - 30 in
  let p = y * (Fb.stride fb) + x * 4 in
  let v = if blink then 0xFF else 0x00 in
  let buf = Fb.buffer fb in
  buf.{p} <- v; buf.{p + 1} <- v; buf.{p + 2} <- v;
  ()

let rec infinite_blink fb blink =
  Gpio.set activity_led blink;
  Serial.write ".";
  (match fb with None -> () | Some fb -> draw_blink fb blink);
  Mtime.sleep_us 1_000_000L;
  infinite_blink fb (not blink)

let greet elapsed_us =
  let elapsed_ms = Int64.to_int elapsed_us / 1000 in
  Serial.write "\r\nWelcome to rpi-boot-ocaml \xF0\x9F\x90\xAB\r\n";
  Serial.writef "Boot time: %dms\r\n" elapsed_ms;
  match Fb.init ~bpp:32 with
  | `Error (`Msg m) -> Serial.writef "Framebuffer: none: %s\r\n" m; None
  | `Ok fb ->
      let w, h, bpp, addr = Fb.(w fb, h fb, bpp fb, addr fb) in
      Serial.writef "Framebuffer: %dx%d bpp:%d addr:0x%nX\r\n" w h bpp addr;
      draw_image fb Logo.img;
      Some fb

let main () =
  let boot_time = Mtime.elapsed_us () in
  let fb = greet boot_time in
  Gpio.set_func activity_led Gpio.F_OUT;
  infinite_blink fb true

let () = try main () with e -> Serial.write (Printexc.get_backtrace ())

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
