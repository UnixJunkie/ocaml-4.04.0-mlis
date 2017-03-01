(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   uuseg release 0.9.0
  ---------------------------------------------------------------------------*)

(** Unicode text segmentation on UTF-X OCaml strings.

    {!Uuseg} helper functions acting directly on UTF-X encoded OCaml
    strings.

    {b Warning.} All these functions silently replace malformed encoded Unicode
    data by a {!Uutf.u_rep} character.

    {e Release 0.9.0 – Daniel Bünzli <daniel.buenzl i\@erratique.ch> } *)

(** {1:segment Segment} *)

type 'a folder = 'a -> string -> 'a
(** The type for segment folders. The function takes an accumulator
    and a segment. Segments are the UTF-X encoded characters delimited
    by two [`Boundary] occurences. If the segmenter has no initial or
    final [`Boundary], the folding function inserts an implicit
    one. Empty segments – which by definition do not happen with
    the default segmenters – are not reported. *)

val fold_utf_8 : [< Uuseg.boundary] -> 'a folder -> 'a -> string -> 'a
(** [fold_utf_8 b f acc s] folds over the [b] UTF-8 encoded segments of
    the UTF-8 encoded string [s] using [f] and [acc]. *)

val fold_utf_16be : [< Uuseg.boundary] -> 'a folder -> 'a -> string -> 'a
(** [fold_utf16be] is like {!fold_utf_8} but on UTF-16BE encoded strings. *)

val fold_utf_16le : [< Uuseg.boundary] -> 'a folder -> 'a -> string -> 'a
(** [fold_utf16le] is like {!fold_utf_8} but on UTF-16BE encoded
    strings. *)

(** {1:pp Pretty-printers}

    Using OCaml's {!Format.pp_print_string} with Unicode encoded
    strings will most of the time derail the pretty-printing process
    for two reasons. First the Unicode encoding of a character may
    span more than one byte and [pp_print_string] considers one
    character to be one byte. Second there may be a discrepancy
    between the sequence of user-perceived characters (grapheme
    clusters e.g. é) and the actual sequences of Unicode characters in
    the data (e.g. é represented by the decomposition e + ´,
    <U+0065,U+0301>).

    The following formatters fix these problems for many (but not all)
    scripts. *)

val pp_utf_8 : Format.formatter -> string -> unit
(** [pp_utf8 ppf s] prints the UTF-8 encoded string [s]. Each grapheme
    cluster is considered as taking a length of 1. *)

val pp_utf_8_text : Format.formatter -> string -> unit
(** [pp_utf_8_text ppf s] prints the UTF-8 encoded string [s]. Each
    grapheme cluster is considered as taking a length of 1. Each
    line break opportunity is hinted with {!Format.pp_print_break}
    and mandatory line breaks issue a {!Format.pp_force_newline} call.

    Take into account the following points:
    {ul
    {- Any {{!Uucp.White.is_white_space}white space} Unicode character
       occuring before a break opportunity will be translated to a space
       (U+0020) in output if no break occurs.}
    {- The sequence CR LF (U+000D, U+000A) and all kind of mandatory
       line breaks are translated to whathever line separator is output
       by {!Format.pp_force_newline}. See {!pp_utf_8_lines} for the
       list of characters treated as mandatory line breaks.}
    {- Soft hyphens are handled but due to limitations in {!Format} are
       not replaced by hard ones on breaks.}} *)

val pp_utf_8_lines : Format.formatter -> string -> unit
(** [pp_utf_8_lines ppf s] prints the UTF-8 encoded string [s]. Each
    grapheme cluster is considered as taking a length of 1. Each
    mandatory line break (including the sequence CR LF (U+000D,
    U+000A)) issues a {!Format.pp_force_newline} and is translated to
    whathever line separator this function outputs.

    This function correctly handles all kinds of line ends present
    Unicode, as of 7.0.0 this is FORM FEED (U+000C), LINE TABULATION
    (U+000B), LINE SEPARATOR (U+2028), PARAGRAPH SEPARATOR (U+2020),
    NEXT LINE (U+085), LINE FEED (U+000A), CARRIAGE RETURN (U+000D),
    and the sequence CR LF (U+000D, U+000A). *)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
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
