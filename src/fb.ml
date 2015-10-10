(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rpi

let strf = Format.asprintf

let tag_allocate_buffer = 0x40001l
let tag_release_buffer  = 0x48001l
let tag_blank_screen    = 0x40002l

let tag_phys_wh     = 0x40003l
let tag_virt_wh     = 0x40004l
let tag_depth       = 0x40005l
let tag_pixel_order = 0x40006l
let tag_alpha_mode  = 0x40007l
let tag_pitch       = 0x40008l
let tag_virt_offset = 0x40009l
let tag_overscan    = 0x4000al
let tag_palette     = 0x4000bl

type op = Get | Test | Set

let tag_op op t = match op with
| Get -> t
| Test -> Int32.logor t 0x04000l
| Set  -> Int32.logor t 0x08000l

let get_size () =
  let phys_wh = Mbox.Prop.(req tag_phys_wh ~resp:int_pair) in
  match Mbox.Prop.(send [r phys_wh]) with
  | `Error _ as e -> e
  | `Ok resp ->
      match Mbox.Prop.find resp phys_wh with
      | `Error _ as e -> e
      | `Ok None -> `Error (`Msg "framebuffer size undefined")
      | `Ok (Some s) -> `Ok s

let set_phys_wh w h =
  Mbox.Prop.(req (tag_op Set tag_phys_wh) ~args:[w;h] ~resp:int_pair)

let set_virt_wh w h =
  Mbox.Prop.(req (tag_op Set tag_virt_wh) ~args:[w;h] ~resp:int_pair)

let set_bpp bpp =
  Mbox.Prop.(req (tag_op Set tag_depth) ~args:[bpp] ~resp:int)

let get_pitch =
  Mbox.Prop.(req tag_pitch ~resp:int32)

let alloc_buffer =
  Mbox.Prop.(req tag_allocate_buffer ~args:[1l] ~resp:int32_pair)

let request_fb w h bpp =
  let w, h, bpp = Int32.(of_int w, of_int h, of_int bpp) in
  let pwh = set_phys_wh w h in
  let vwh = set_virt_wh w h in
  let bpp = set_bpp bpp in
  let buf = alloc_buffer in
  let pitch = get_pitch in
  let reqs = Mbox.Prop.[r pwh; r vwh; r bpp; r buf; r pitch] in
  match Mbox.Prop.(send reqs)  with
  | `Error _ as e -> e
  | `Ok resp ->
      match Mbox.Prop.find resp pitch with
      | `Error (`Msg m) -> failwith m
      | `Ok None -> failwith "framebuffer pitch undefined"
      | `Ok (Some pitch) ->
          match Mbox.Prop.find resp buf with
          | `Error (`Msg m) -> failwith m
          | `Ok None -> failwith "framebuffer buffer undefined"
          | `Ok (Some (a, l)) ->
              `Ok (Mem.of_int32 a, Int32.to_int l, Int32.to_int pitch)

type t =
    { w : int; h : int; stride : int;
      bpp : int; buffer : Mem.Map.bytes; }

let bus_address_to_arm_physical addr = Nativeint.sub addr 0xC0000000n

let init ~bpp = match get_size () with
| `Error _ as e -> e
| `Ok (w, h) ->
    match request_fb w h bpp with
    | `Error _ as e -> e
    | `Ok (addr, len, stride) ->
        let addr = bus_address_to_arm_physical addr in
        let buffer = Mem.Map.bytes addr ~len in
        `Ok { w; h; stride; bpp; buffer }

let w fb = fb.w
let h fb = fb.h
let stride fb = fb.stride
let bpp fb = fb.bpp
let buffer fb = fb.buffer
let addr fb = Mem.Map.base fb.buffer

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
