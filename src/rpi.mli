(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Rasbperry PI hardware access.

    Access to {{!Mem}memory}, {{!Gpio}GPIO pins}, {{!Mbox}mailboxes},
    {{!Mtime}monotonic time}, and {{!Serial}a serial connection}.

   {b References}
   {ul
   {- {{:https://www.raspberrypi.org/documentation/hardware/}
      The Raspberry Pi Hardware documentation}.}
   {- {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf}BCM2835 ARM Peripheral specification} (PDF). Also
      applies to RPiv2, except the MMIO base ARM physical address is
      different.}} *)

(**   {1:low Low-level interfaces} *)

type 'a result = [ `Ok of 'a | `Error of [`Msg of string ]]
(** The type for results. FIXME use [rresult]. *)

(** Memory.

    {b Note.} All multi-bytes memory accesses are done in little endian
    order. *)
module Mem : sig

  (** {1:addr Addresses} *)

  type addr = nativeint
  (** The type for byte addresses. *)

  val ( + ) : addr -> addr -> addr
  (** [a + off] adds [off] to [a]. *)

  val ( - ) : addr -> addr -> addr
  (** [a - off] subracts [off] to [a]. *)

  val offset : addr -> int -> addr
  (** [offset addr n] is [add + Nativeint.of_int n]. *)

  val of_int32 : int32 -> addr
  (** [of_int32 i] is the address corresponding to [i]. *)

  val pp_addr : Format.formatter -> addr -> unit
  (** [pp_addr ppf a] prints and unspecified reprsentation of [a]
      on [ppf]. *)

  (** {1:barriers Memory barriers} *)

  val wait : int -> unit
  (** [wait n] waits at least [n] CPU cycles. *)

  val dsb : unit -> unit
  (** [dsb ()] performs a data synchronization barrier. Returns when
      all instructions before the call are complete. *)

  val dmb : unit -> unit
  (** [dmb ()] performs a data memory barrier. Ensures that all explicit
      memory access before the call complete before any new explicit
      access made after the call. *)

  val isb : unit -> unit
  (** [isb ()] performs an instruction synchronization barrier. Flushes
      the pipeline in the processor so that all instruction following the
      call are fetched from cache or memory. *)

  (** {1:reads Reads} *)

  val get : addr -> int
  (** [get a] gets the byte at address [a]. *)

  val get_int : addr -> int
  (** [get_int a] gets the 4 bytes starting at address [a].

      {b Warning.} Truncates the value of the 32nd bit. *)

  val get_int32 : addr -> int32
  (** [get_int32 a] gets the 4 bytes starting at address [a]. *)

  val get_int64 : addr -> int64
  (** [get_int64 a] gets the 8 bytes starting at address [a]. *)

  (** {1:writes Writes} *)

  val set : addr -> int -> unit
  (** [set a v] sets the byte at address [a] to [v]. *)

  val set_int : addr -> int -> unit
  (** [set a v] sets the 4 bytes starting at address [a] to [v]. *)

  val set_int32 : addr -> int32 -> unit
  (** [set a v] sets the 4 bytes starting at address [a] to [v]. *)

  val set_int64 : addr -> int64 -> unit
  (** [set a v] sets the 8 bytes starting at address [a] to [v]. *)

  (** {1:mask Masked writes} *)

  val set_bits : addr -> bits:int -> int -> unit
  (** [set_bits] is like {!set} but only affects the bits set
      in [bits]. *)

  val set_int_bits : addr -> bits:int -> int -> unit
  (** [masked_set_int] is like {!set_int} but only affects the bits set
      in [bits]. *)

  val set_int32_bits : addr -> bits:int32 -> int32 -> unit
  (** [set_int32_bits] is like {!set_int32} but only affects the bits
      set in [bits]. *)

  val set_int64_bits : addr -> bits:int64 -> int64 -> unit
  (** [set_int64_bits] is like {!set_int64} but only affects the bits
      set in [bits]. *)

  (** {1:map Mapping} *)

  (** Memory maps *)
  module Map : sig

    (** {1 Maps} *)

    type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
    (** The type for memory maps. *)

    type bytes = (int, Bigarray.int8_unsigned_elt) t
    (** The type for byte memory maps. *)

    type int32s = (int32, Bigarray.int32_elt) t
    (** The type for int32 memory maps. *)

    type int64s = (int64, Bigarray.int64_elt) t
    (** The type for int64 memory maps. *)

    val length : ('a ,'b) t -> int
    (** [length m] is [m]'s scalar length. *)

    val byte_length : ('a, 'b) t -> int
    (** [byte_length m] is [m]'s byte length. *)

    val base : ('a, 'b) t -> addr
    (** [base m] is [m]'s base address. *)

    val bytes : addr -> len:int -> bytes
    (** [bytes a len] maps [len] bytes starting at [a]. *)

    val int32s : addr -> len:int -> int32s
     (** [int32s a len] maps [len] int32 values starting at [a]. *)

    val int64s : addr -> len:int -> int64s
    (** [map_int64 a len] maps [len] int64 values starting at [a]. *)
  end
end

(** Memory mapped IO. *)
module Mmio : sig

  (** {1:base Base address} *)

  val base : Mem.addr
  (** The base ARM physical address at which memory mapped IO is available.
      On a RPiv2 this is [0x3F000000]. On previous models (unsupported
      for now) it was [0x20000000]. *)
end

(** GPIO pins.

    {b References}
    {ul
    {- {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/gpio/README.md}GPIO Raspberry Pi documentation}.}
    {- Section 6 of the {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf}BCM2835 ARM Peripheral specification} (PDF).}} *)
module Gpio : sig

  (** {1:pins Pins} *)

  (** The type for GPIO pins. *)
  type pin =
  | P00 | P01 | P02 | P03 | P04 | P05 | P06 | P07 | P08 | P09
  | P10 | P11 | P12 | P13 | P14 | P15 | P16 | P17 | P18 | P19
  | P20 | P21 | P22 | P23 | P24 | P25 | P26 | P27 | P28 | P29
  | P30 | P31 | P32 | P33 | P34 | P35 | P36 | P37 | P38 | P39
  | P40 | P41 | P42 | P43 | P44 | P45 | P46 | P47 | P48 | P49
  | P50 | P51 | P52 | P53

  (** {1:setup Pin setup} *)

  (** The type for pin functions. *)
  type func =
  | F_IN | F_OUT | F_ALT5 | F_ALT4 | F_ALT0 | F_ALT1 | F_ALT2 | F_ALT3

  val set_func : pin -> func -> unit
  (** [set p func] sets the function of pin [p] to [func]. *)

  (** The type for pin pull state. *)
  type pull_state = PULL_OFF | PULL_DOWN | PULL_UP

  val set_pull_state : pin -> pull_state -> unit
  (** [set p state] sets the pull state of pin [p] to [state]. *)

  (** {1:rw Read and write} *)

  val get : pin -> bool
  (** [get p] is the current value of pin [p]. *)

  val set : pin -> bool -> unit
  (** [set p v] sets the value of pin [p] to [v]. *)
end

(** Mailboxes.

    Mailbox allow to connect with the RPi's GPU which is in charge
    of a lot the bookkeeping.

    This is not very well documented, a bit of documentation can be found {{:https://github.com/raspberrypi/firmware/wiki/Mailboxes}here}.
    Using the {{!propi}property} interface should be enough. *)
module Mbox : sig

  (** {1:propi Property interface} *)

  (** Property interface

      Convenience interface for the
      {{:https://github.com/raspberrypi/firmware/wiki/Mailbox-property-interface}mailbox property interface} (channel [Tags_ARM_to_VC]). *)
  module Prop : sig

    (** {1 Property values} *)

    (** The type for response property values. The expected maximal
        length of the response and a property parser. The map given to
        the property parser can be shorter than the specified
        length. *)
    type 'a t =
    | Unit : unit t
    | Bytes : int * (Mem.Map.bytes -> 'a result) -> 'a t
    | Int32 : int * (Mem.Map.int32s -> 'a result) -> 'a t
    | Int64 : int * (Mem.Map.int64s -> 'a result) -> 'a t

    val unit : unit t
    (** [unit] is an empty property. *)

    val string : max:int -> string t
    (** [string] is a string of maximum length [max]. *)

    val int : int t
    (** [int] is an integer property (parsed from an int32). *)

    val int32 : int32 t
    (** [int32] is an int32 property. *)

    val int64 : int64 t
    (** [int64] is an int64 property. *)

    val int_pair : (int * int) t
    (** [int] is an integer pair property (parsed from two int32s). *)

    val int32_pair : (int32 * int32) t
    (** [int32_pair] is an int32 pair property. *)

    val int64_pair : (int64 * int64) t
    (** [int32_pair] is an int64 pair property. *)

    (** {1:req Requests} *)

    type args = int32 list
    (** The type for property requests arguments. FIXME this
        should maybe be generalized but at the moment all published
        requests arguments are uint32. *)

    type 'a req
    (** The type for requests of properties of type 'a. *)

    type ereq
    (** The type for existential requests. *)

    val req : ?args:args -> int32 -> resp:'a t -> 'a req
    (** [req t args resp] is a property for tag [t] with request arguments
        [args] (defaults to []) and response property parsed with [resp]. *)

    val r : 'a req -> ereq
    (** [r req] is an existential request for [r]. *)

    (** {1:resp Responses} *)

    type resp
    (** The type for responses. *)

    val send : ereq list -> resp result
    (** [send reqs] sends the list of requests [reqs] in the given order. *)

    val find : resp -> 'a req -> 'a option result
    (** [find resp req] finds the property value of [req] in response [resp].
        [None] is returned if the property can't be found. *)
  end

  (** {1:channels Channels} *)

  (** The type for mailbox channels *)
  type channel =
  | Power_management | Framebuffer | Virtual_UART | VCHIQ | LEDs
  | Buttons | Touchscreen | Unused | Tags_ARM_to_VC | Tags_VC_to_ARM

  (** {1:addr Communicating addresses}

      {b Warning} The lowest 4-bits of sent addresses can't be read
      by the GPU as they are used to transmit the channel.
      used. As such addresses must be aligned on 16 bytes. *)

  val read : channel -> Mem.addr
  (** [read c] reads the address of channel [c]. *)

  val write : channel -> Mem.addr -> unit
  (** [write c a] writes the [v] to the channel [c]. *)
end

(** {1 Higher-level interfaces} *)

(** Monotonic time.

    [Mtime] gives access to the 64-bit free running system timer
    counter. Note that this time is independent from the CPU speed.

    {b References}
    {ul
    {- Section 12 of the {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf}BCM2835 ARM Peripheral specification} (PDF).}} *)
module Mtime : sig

  (** {1:spans Time spans} *)

  type span_us = int64
  (** The type for time spans in {e unsigned} microseconds. *)

  (** {1:passing Passing time} *)

  val elapsed_us : unit -> span_us
  (** [elapsed ()] is the number of microseconds elasped since boot
      time. *)

  val sleep_us : span_us -> unit
  (** [sleep_us d] blocks and sleeps for [d] microseconds. *)

  (** {1:counters Counters} *)

  type counter
  (** The type for counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting from call time on. *)

  val counter_value_us : counter -> span_us
  (** [counter_value_us c] is the current counter value in microseconds. *)

  (** {1:conv Time conversion} *)

  val s_to_us : int64
  (** [s_to_us] is the number of microseconds in one second. *)

  val ms_to_us : int64
  (** [ms_to_us] is the number of microseconds in one millisecond. *)
end

(** Serial connection.

    [Serial] gives access to a
    {{:http://elinux.org/RPi_Serial_Connection} serial connection}
    using the Pi's UART0 peripheral.

    {b References}
    {ul
    {- Section 13 of the {{:https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2835/BCM2835-ARM-Peripherals.pdf}BCM2835 ARM Peripheral specification} (PDF).}} *)
module Serial : sig

  (** {1:init Initialization} *)

  val init : unit -> unit
  (** [init ()] initializes the serial connection. *)

  (** {1:read Read} *)

  val read_byte : unit -> int
  (** [read_byte ()] blocks until a byte becomes available on the
      serial connection. *)

  val try_read_byte : unit -> int option
  (** [try_read_byte ()] is [Some b] a byte could be read from the
      serial connection and [None] otherwise. *)

  (** {1:write Write} *)

  val write_byte : int -> unit
  (** [write_byte b] writes the byte [b] on the serial connection. *)

  val write : string -> unit
  (** [write s] writes [s] on the serial connection. *)

  val writef :  ('a, Format.formatter, unit) format -> 'a
  (** [writef fmt ...] write a string formatted according to [fmt]
      on the serial connection. *)
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
