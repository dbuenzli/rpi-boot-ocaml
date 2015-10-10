(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf

type 'a result = [ `Ok of 'a | `Error of [`Msg of string ] ]

module Mem = struct

  (* Addresses and memory barriers *)

  type addr = nativeint

  external wait : int -> unit = "ocamlrpi_barrier_wait" "noalloc"
  external dsb : unit -> unit = "ocamlrpi_barrier_dsb" "noalloc"
  external dmb : unit -> unit = "ocamlrpi_barrier_dmb" "noalloc"
  external isb : unit -> unit = "ocamlrpi_barrier_isb" "noalloc"

  let ( + ) = Nativeint.add
  let ( - ) = Nativeint.sub
  let offset a off = Nativeint.(add a (of_int off))
  let of_int32 = Nativeint.of_int32
  let pp_addr ppf a = Format.fprintf ppf "0x%nX" a

  (* Reads *)

  external get : addr -> int = "ocamlrpi_mem_get_byte" "noalloc"
  external get_int : addr -> int = "ocamlrpi_mem_get_int" "noalloc"
  external get_int32 : addr -> int32 = "ocamlrpi_mem_get_int32"
  external get_int64 : addr -> int64 = "ocamlrpi_mem_get_int64"

  (* Writes *)

  external set : addr -> int -> unit =
    "ocamlrpi_mem_set_byte" "noalloc"

  external set_int : addr -> int -> unit =
    "ocamlrpi_mem_set_int" "noalloc"

  external set_int32 : addr -> int32 -> unit =
    "ocamlrpi_mem_set_int32" "noalloc"

  external set_int64 : addr -> int64 -> unit =
    "ocamlrpi_mem_set_int64" "noalloc"

  (* Masked writes *)

  external set_bits : addr -> bits:int -> int -> unit =
    "ocamlrpi_mem_set_byte_bits" "noalloc"

  external set_int_bits : addr -> bits:int -> int -> unit =
    "ocamlrpi_mem_set_int_bits" "noalloc"

  external set_int32_bits : addr -> bits:int32 -> int32 -> unit =
    "ocamlrpi_mem_set_int32_bits" "noalloc"

  external set_int64_bits : addr -> bits:int64 -> int64 -> unit =
    "ocamlrpi_mem_set_int64_bits" "noalloc"

  external set_int32_pow : addr -> int -> unit =
    "ocamlrpi_mem_set_int32_pow" "noalloc"

  (* Mapping *)

  module Map = struct

    type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
    type bytes = (int, Bigarray.int8_unsigned_elt) t
    type int32s = (int32, Bigarray.int32_elt) t
    type int64s = (int64, Bigarray.int64_elt) t

    let length = Bigarray.Array1.dim

    external byte_length : ('a, 'b) t -> int =
      "ocamlrpi_mem_map_byte_length"

    external base : ('a, 'b) t -> addr =
      "ocamlrpi_mem_map_base"

    external bytes : addr -> len:int -> bytes =
      "ocamlrpi_mem_map_bytes"

    external int32s : addr -> len:int -> int32s =
      "ocamlrpi_mem_map_int32"

    external int64s : addr -> len:int -> int64s =
      "ocamlrpi_mem_map_int64"
  end
end

module Mmio = struct
  external get_base : unit -> nativeint = "ocamlrpi_mmio_base"
  let base = get_base ()
end

module Gpio = struct

  (* GPIO addresses *)

  let gp_base = Mem.(Mmio.base + 0x00200000n)
  let gp_sel0 = Mem.(gp_base + 0x00n)
  let gp_set0 = Mem.(gp_base + 0x1Cn)
  let gp_set1 = Mem.(gp_base + 0x20n)
  let gp_clr0 = Mem.(gp_base + 0x28n)
  let gp_clr1 = Mem.(gp_base + 0x2Cn)
  let gp_pud = Mem.(gp_base + 0x94n)
  let gp_pudclk0 = Mem.(gp_base + 0x98n)
  let gp_pudclk1 = Mem.(gp_base + 0x98n)

  (* Pins *)

  type pin =
  | P00 | P01 | P02 | P03 | P04 | P05 | P06 | P07 | P08 | P09
  | P10 | P11 | P12 | P13 | P14 | P15 | P16 | P17 | P18 | P19
  | P20 | P21 | P22 | P23 | P24 | P25 | P26 | P27 | P28 | P29
  | P30 | P31 | P32 | P33 | P34 | P35 | P36 | P37 | P38 | P39
  | P40 | P41 | P42 | P43 | P44 | P45 | P46 | P47 | P48 | P49
  | P50 | P51 | P52 | P53

  let pin_to_int : pin -> int = fun p -> Obj.magic (Obj.repr p)

  (* Setup *)

  type func = F_IN | F_OUT | F_ALT5 | F_ALT4 | F_ALT0 | F_ALT1 | F_ALT2 | F_ALT3

  let func_to_int : func -> int = fun f -> Obj.magic (Obj.repr f)

  let set_func p f =
    let p = pin_to_int p in
    let f = func_to_int f in
    let r = Mem.offset gp_sel0 (4 * (p / 10)) in
    let bit_start = (p mod 10) * 3 in
    Mem.set_int_bits r ~bits:(0b111 lsl bit_start) (f lsl bit_start);
    ()

  type pull_state = PULL_OFF | PULL_DOWN | PULL_UP

  let pull_state_to_int : pull_state -> int = fun s -> Obj.magic (Obj.repr s)

  let set_pull_state p s =
    let p = pin_to_int p in
    let s = pull_state_to_int s in
    let clk, n = if p > 31 then gp_pudclk1, p land 31 else gp_pudclk0, p in
    Mem.set_int gp_pud s;
    Mem.wait 150;
    Mem.set_int32_pow clk n;
    Mem.wait 150;
    Mem.set_int gp_pud 0;
    Mem.set_int clk 0;
    ()

  (* Read and write *)

  let get p = failwith "TODO"
  let set p b =
    let p = pin_to_int p in
    let r, n =
      if p > 31
      then (if b then gp_set1 else gp_clr1), (p land 31)
      else (if b then gp_set0 else gp_clr0), p
    in
    Mem.set_int32_pow r n;
    ()
end

(* We define serial as soon as possible so that it's available for
   debugging in this module. *)

module Serial = struct

  (* UART0 registers *)

  let uart_base = Mem.(Mmio.base + 0x00201000n)
  let uart_dr = Mem.(uart_base + 0x00n)
  let uart_fr = Mem.(uart_base + 0x18n)
  let uart_ibrd = Mem.(uart_base + 0x24n)
  let uart_fbrd = Mem.(uart_base + 0x28n)
  let uart_lcrh = Mem.(uart_base + 0x2cn)
  let uart_cr = Mem.(uart_base + 0x30n)
  let uart_imsc = Mem.(uart_base + 0x38n)
  let uart_icr = Mem.(uart_base + 0x44n)

  (* Initialisation *)

  let inited = ref false

  let init () =
    if !inited then () else
    begin
      (* Disable UART *)
      Mem.set_int uart_cr 0;
      (* Disable pull up/down resistors on RC and TX pins *)
      Gpio.(set_pull_state P14 PULL_OFF);
      Gpio.(set_pull_state P15 PULL_OFF);
      (* Clear interrupts *)
      Mem.set_int uart_icr 0x7FF;
      (* Set baud rate to 115200 *)
      Mem.set_int uart_ibrd 1;
      Mem.set_int uart_fbrd 40;
      (* FIFO, 8 bit, no parity *)
      Mem.set_int uart_lcrh ((1 lsl 4) lor (0b11 lsl 5));
      (* Mask interrupts *)
      Mem.set_int32 uart_imsc 0xFFFFl;
      (* Enable UART with reception and transmission *)
      Mem.set_int uart_cr ((1 lsl 0) lor (1 lsl 8) lor (1 lsl 9));
      inited := true
    end

  (* Reads *)

  let read_byte () =
    while (Mem.get_int uart_fr land (1 lsl 4) <> 0) do () done;
    Mem.get_int uart_dr land 0xFF

  let try_read_byte () = match Mem.get_int uart_fr land (1 lsl 4) with
  | 0 -> None
  | n -> Some (Mem.get_int uart_dr land 0xFF)

  (* Writes *)

  let write_byte byte =
    while (Mem.get_int uart_fr land (1 lsl 5) <> 0) do () done;
    Mem.set_int uart_dr byte

  let write s =
    let max = String.length s - 1 in
    for i = 0 to max do write_byte (Char.code (String.unsafe_get s i)) done

  let b = Buffer.create 256
  let ppf = Format.formatter_of_buffer b
  let writef fmt =
    let k ppf =
      Format.pp_print_flush ppf ();
      let c = Buffer.contents b in
      (Buffer.clear b; write c)
    in
    Format.kfprintf k ppf fmt
end

module Mbox = struct

  type channel =
  | Power_management | Framebuffer | Virtual_UART | VCHIQ | LEDs
  | Buttons | Touchscreen | Unused | Tags_ARM_to_VC | Tags_VC_to_ARM

  let channel_to_int32 : channel -> int32 =
    fun m -> Int32.of_int (Obj.magic (Obj.repr m) : int)

  let mbox_base = Mem.(Mmio.base + 0xB880n)
  let mbox_read = Mem.(mbox_base + 0x00n)
  let mbox_peek = Mem.(mbox_base + 0x10n)
  let mbox_sender = Mem.(mbox_base + 0x14n)
  let mbox_status = Mem.(mbox_base + 0x18n)
  let mbox_config = Mem.(mbox_base + 0x1Cn)
  let mbox_write = Mem.(mbox_base + 0x20n)

  let empty = 0x40000000l
  let full = 0x80000000l

  let msg channel v = Int32.(logor v channel)
  let msg_channel r = Int32.logand r 0xFl
  let msg_value r = Nativeint.of_int32 (Int32.(logand r 0xFFFF_FFF0l))

  let block_on s =
    while Int32.logand (Mem.get_int32 mbox_status) s <> 0l do () done

  let read c =
    let c = channel_to_int32 c in
    let rec loop () =
      block_on empty;
      Mem.dmb ();
      let m = Mem.get_int32 mbox_read in
      Mem.dmb ();
      if Int32.compare (msg_channel m) c = 0 then msg_value m else
      loop ()
    in
    loop ()

  let write c v =
    let c = channel_to_int32 c in
    let v = Nativeint.to_int32 v in
    let rec loop () =
      block_on full;
      Mem.set_int32 mbox_write (msg c v);
    in
    loop ()

  (* Property interface *)

  module Prop = struct

    (* Property values *)

    type 'a t =
    | Unit : unit t
    | Bytes : int * (Mem.Map.bytes -> 'a result) -> 'a t
    | Int32 : int * (Mem.Map.int32s -> 'a result) -> 'a t
    | Int64 : int * (Mem.Map.int64s -> 'a result) -> 'a t

    let prop_byte_length = fun (type a) (p : a t) -> match p with
    | Unit -> 0
    | Bytes (count, _) -> count
    | Int32 (count, _) -> 4 * count
    | Int64 (count, _) -> 8 * count

    let err_short len exp =
      `Error (`Msg (strf "response value too short exp:%d got:%d" exp len))

    let id x = x

    let get1 conv m =
      let len = Mem.Map.length m in
      if len < 1 then err_short len 1 else `Ok (conv m.{0})

    let get2 conv m =
      let len = Mem.Map.length m in
      if len < 2 then err_short len 2 else
      `Ok (conv m.{0}, conv m.{1})

    let unit = Unit
    let int = Int32 (1, get1 Int32.to_int)
    let int32 = Int32 (1, get1 id)
    let int64 = Int64 (1, get1 id)
    let int_pair = Int32 (2, get2 Int32.to_int)
    let int32_pair = Int32 (2, get2 id)
    let int64_pair = Int64 (2, get2 id)

    let string ~max =
      let parse m =
        let len = Mem.Map.length m in
        let b = Bytes.create len in
        let max = len - 1 in
        for i = 0 to max do Bytes.unsafe_set b i (Char.unsafe_chr m.{i}) done;
        `Ok (Bytes.unsafe_to_string b)
      in
      Bytes (max, parse)

    (* Requests *)

    type args = int32 list
    type ereq = { tag : int32; args : args; buf_blen : int; }
    type 'a req = ereq * 'a t

    let req ?(args = []) tag ~resp:prop =
      let buf_blen = max (4 * List.length args) (prop_byte_length prop) in
      let buf_blen =
        let rem = buf_blen mod 4 in
        if rem = 0 then buf_blen else buf_blen + (4 - rem)
      in
      { tag; args; buf_blen }, prop

    let req_byte_length r =
      4 (* tag *) + 4 (* val len *) + 4 (* indicator *) + r.buf_blen

    let r (ereq, _) = ereq

    (* Responses *)

    type resp = { msg : Mem.Map.int32s;
                  (* Tag, (start of value in msg, byte length) *)
                  tag_index : (Int32.t * (int * int)) list; }

    let msg_addr = 0x1000n
     (* The location is free but FIXME. This should be allocated via
        malloc on a 16 byte boundary. A function should be added for
        that in Mem. It must be possible to do that by simply
        allocating a bigarray with Bigarray.Array1.create with + 16
        bytes an extracting a sub map out of it.

        FIXME Besides should this address be sent as a VC CPU Bus
        address ? Unclear. *)

    let request_msg reqs =
      let add_req_blen acc r = acc + req_byte_length r in
      let reqs_blen = List.fold_left add_req_blen 0 reqs in
      let msg_blen = 4 (* size *) + 4 (* code *) + reqs_blen + 4 (* end *) in
      let m = Mem.Map.int32s msg_addr ~len:(msg_blen / 4) in
      let rec add_reqs i = function
      | [] -> m.{i} <- 0l (* end tag *)
      | r :: reqs ->
          let next = i + 2 + r.buf_blen / 4 + 1 in
          m.{i    } <- r.tag;
          m.{i + 1} <- Int32.of_int r.buf_blen;
          m.{i + 2} <- Int32.of_int @@ 4 * List.length r.args;
          ignore (List.fold_left (fun i a -> m.{i} <- a; i + 1) (i + 3) r.args);
          add_reqs next reqs
      in
      m.{0} <- Int32.of_int msg_blen;
      m.{1} <- 0l; (* Request *)
      add_reqs 2 reqs;
      m

    let err_unterminated = `Msg "unterminated response"
    let err_truncated_resp t = `Msg (strf "truncated response (tag:%lX)" t)
    let err_unknown_code c = `Msg (strf "unknown reponse code: %lX" c)
    let err_parse = `Msg (strf "GPU: error parsing request")
    let err_addr a a' =
      `Msg (strf "different return address %a %a" Mem.pp_addr a Mem.pp_addr a')

    let tag_index m =
      let max = Mem.Map.length m - 1 in
      let rec loop acc i =
        if i > max then `Error err_unterminated else
        let tag = m.{i} in
        if tag = 0l then `Ok acc else
        if i + 2 > max then `Error (err_truncated_resp tag) else
        let buf_blen = m.{i + 1} (* n.b. this is multiple of 4. *) in
        let next = i + 2 + (Int32.to_int buf_blen / 4) + 1 in
        let indic = m.{i + 2} in
        let acc =
          if (Int32.logand 0x8000_0000l indic = 0l) then acc else
          (tag, (i + 3, Int32.(to_int (logand 0x7FFF_FFFFl indic)))) :: acc
        in
        loop acc next
      in
      loop [] 2

    let send reqs =
      let msg = request_msg reqs in
      let addr = Mem.Map.base msg in
      write Tags_ARM_to_VC addr;
      let addr' = read Tags_ARM_to_VC in
      if addr <> addr' then `Error (err_addr addr addr') else
      match msg.{1} with
      | 0x80000000l ->
          begin match tag_index msg with
          | `Ok tag_index -> `Ok {msg; tag_index}
          | `Error _ as e -> e
          end
      | 0x80000001l -> `Error err_parse
      | c -> `Error (err_unknown_code c)

    let find (type a) resp (ereq, (prop : a t)) : a option result =
      try
        let some = function `Ok v -> `Ok (Some v) | `Error _ as e -> e in
        let pos, len = List.assoc ereq.tag resp.tag_index in
        match prop with
        | Unit -> `Ok (Some ())
        | Bytes (_, parse) ->
            let addr = Mem.(offset (Map.base resp.msg) @@ 4 * pos) in
            some @@ parse (Mem.Map.bytes addr len)
        | Int32 (_, parse) ->
            some @@ parse (Bigarray.Array1.sub resp.msg pos (len / 4))
        | Int64 (_, parse) ->
            let addr = Mem.(offset (Map.base resp.msg) @@ 4 * pos) in
            some @@ parse (Mem.Map.int64s addr (len / 8))
      with Not_found -> `Ok None

  end
end

module Mtime = struct

  (* Time spans *)

  type span_us = int64

  (* Passing time *)

  let timer_base = Mem.(Mmio.base + 0x00003000n)
  let timer_clo = Mem.(timer_base + 0x04n)

  let elapsed_us () = Mem.get_int64 timer_clo
  let sleep_us d =
    (* That's a bit wasteful and unprecise because of allocs, FIXME
       wfi + timer IRQ *)
    let rec loop start =
      let e = Int64.sub (elapsed_us ()) start in
      if Int64.compare e d < 0 then loop start else ()
    in
    loop (elapsed_us ())

  (* Counters *)

  type counter = span_us
  let counter = elapsed_us
  let counter_value_us c = Int64.sub (elapsed_us ()) c

  (* Time scale conversions *)

  let s_to_us = 1_000_000L
  let ms_to_us = 1_000L
end

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
