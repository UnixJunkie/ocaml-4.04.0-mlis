(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

(* Runtime support for JSON-XML-Protobuf-Piq serialization
 *
 * This module is used by OCaml modules generated by
 * "piqic-ocaml --multi-format" Piqi compiler
 *)


type input_format = [ `json | `pb | `piq | `pib | `xml ]

type output_format = [ input_format | `json_pretty | `xml_pretty ]

type piqi_type

type options


val init_piqi : string -> unit

val find_piqi_type : string -> piqi_type


(* Construct serialization options to be passed as an optional argument to
 * gen_<typename> and parse_<typename> functions. Available options:
 *
 * pretty_print
 *
 *      Pretty-print generated JSON and XML output (default = true)
 *
 * json_omit_missing_fields
 *
 *      Omit missing optional and empty repeated fields from JSON
 *      output instead of representing them as {"field_name": null} and
 *      {"field_name", []} JSON fields (default = true)
 *
 * use_strict_parsing
 *
 *      Treat unknown and duplicate fields as errors when parsing JSON,
 *      XML and Piq formats (default = false)
 *
 * piq_frameless_output
 *
 *      Print a frame (i.e. :<typename> []) around a single output Piq object
 *      (default=false)
 *
 * piq_frameless_input
 *
 *      Expect a frame around a single input Piq object (default=false)
 *
 * piq_relaxed_parsing
 *
 *      Parse Piq format using "relaxed" mode (default=false);
 *
 *      For instance, when set to `true`, single-word string literals don't have
 *      to be quoted
 *)
val make_options:
        ?pretty_print:bool ->
        ?json_omit_missing_fields:bool ->
        ?json_omit_null_fields:bool -> (* deprecated: use json_omit_missing_fields instead *)
        ?use_strict_parsing:bool ->
        ?piq_frameless_output:bool ->
        ?piq_frameless_input:bool ->
        ?piq_relaxed_parsing:bool ->
        unit -> options

val convert:
  ?opts:options ->
  piqi_type -> input_format -> output_format -> string -> string
