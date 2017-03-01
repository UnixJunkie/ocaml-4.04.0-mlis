(* $Id: xstr_match.mli,v 1.2 1999/07/04 20:02:08 gerd Exp $
 * ----------------------------------------------------------------------
 * Matching strings
 *)

(* Copyright 1999 by Gerd Stolpmann *)

type variable
   (* A 'variable' can record matched regions *)

type charset
   (* sets of characters *)

type matcher =
    Literal of string
  | Anystring 
  | Lazystring 
  | Anychar
  | Anystring_from of charset
  | Lazystring_from of charset
  | Anychar_from of charset
  | Nullstring
  | Alternative of matcher list list
  | Optional of matcher list
  | Record of (variable * matcher list)
  | Scanner of (string -> int)
;;

(* Literal s:            matches literally s and nothing else
 * Anystring/Lazystring  matches a string of arbitrary length with arbitrary
 *                       contents
 * Anystring_from s/    
 * Lazystring_from s     matches a string of arbitrary length with characters
 *                       from charset s
 * Anychar:              matches an arbitrary character
 * Anychar_from s:       matches a character from charset s
 * Nullstring:           matches the empty string
 * Alternative 
 *   [ ml1; ml2; ... ]
 *                    first tries the sequence ml1, then ml2, and so on
 *                    until one of the sequences leads to a match of the
 *                    whole string
 * Optional ml:       first tries the sequence ml, them the empty string.
 *                    = Alternative [ml; [Nullstring]]
 * Record (v, ml):    matches the same as ml, but the region of the string
 *                    is recorded in v
 * Scanner f:         f s is called where s is the rest to match. The function
 *                    should return the number of characters it can match,
 *                    or raise Not_found
 *)


val match_string : matcher list -> string -> bool

  (* match_string ml s:
   * Tries to match 'ml' against the string 's'; returns true on success, and
   * false otherwise.
   * As side-effect, the variables in 'ml' are set.
   * Matching proceeds from left to right, and for some of the matchers there
   * are particular matching orders. The first match that is found using
   * this order is returned (i.e. the variables get their values from this
   * match).
   * Notes:
   * - Anystring and Anystring_from are "greedy"; they try to match as much
   *   as possible.
   * - In contrast to this, Lazystring and Lazystring_from are "lazy"; they
   *   try to match as few as possible.
   * - Alternatives are tested from left to right.
   * - Options are first tested with argument, then with the empty string
   *   (i.e. "greedy")
   *)

type replacer =
    ReplaceLiteral of string
  | ReplaceVar of variable
  | ReplaceFunction of (unit -> string)
;;


type rflag =
    Anchored
  | Limit of int
  (* | RightToLeft *)
;;

val replace_matched_substrings : matcher list -> replacer list -> rflag list
                                  -> string -> (string * int)

  (* replace_matched_substrings ml rl fl s:
   *
   * All substrings of 's' are matched against 'ml' in turn, and all
   * non-overlapping matchings are replaced according 'rl'. The standard
   * behaviour is to test from left to right, and to replace all occurences
   * of substrings.
   * This can be modified by 'fl':
   *   - Anchored:  Not the substrings of 's', but only 's' itself is 
   *                matched against 'ml'. 
   *   - Limit n:   At most 'n' replacements will be done.
   *   - RightToLeft:  Begin with the rightmost matching; proceed with more
   *                   left matchings (NOT YET IMPLEMENTED!!!!)
   * The meaning of 'rl': Every matching is replaced by the sequence of
   * the elements of 'rl'.
   *   - ReplaceLiteral t:  Replace the string t
   *   - ReplaceVar v:      Replace the contents of 'v' or the empty string,
   *                        if v has no matching
   *   - ReplaceFunction f: Replace f(). You may raise Not_found or
   *        Match_failure to skip to the next matching.
   * 'replace_matched_substrings' returns the number of replacements.
   *)


val var : string -> variable

  (* var s: creates new variable with initial value s. If this variable
   * is used in a subsequent matching, and a value is found, the value
   * is overwritten; otherwise the old value persists.
   * - Initial vales are stored as references to strings
   * - Matched values are stored as triples (s,from,len) where 's' is the
   *   input string of the matching function
   *
   * [Note thread-safety: variables must not be shared by multiple threads.]
   *)

val var_matched  : variable -> bool

  (* returns true if the variable matched a value in the last match_string *)

val string_of_var : variable -> string

  (* returns the current value of the variable *)

val found_string_of_var : variable -> string

  (* returns the current value of the variable only if there was a match
   * for this variable in the last match_string; otherwise raise Not_found 
   *)

val mkset : string -> charset

  (* creates a set from readable description. The string simply enumerates
   * the characters of the set, and the notation "x-y" is possible, too.
   * To include '-' in the set, put it at the beginning or end.
   *)

val mknegset : string -> charset

  (* creates the complement that mkset would create *)


(* ---------------------------------------------------------------------- *)

(* EXAMPLE:
 *
 * let v = var "" in
 * let _ = match_string [ Literal "("; Record (v, [Anystring]); Literal ")" ]
 *                      s 
 * in found_string_of_var v
 *
 * - if s is "(abc)" returns "abc"
 * - if the parantheses are missing, raises Not_found
 *
 * VARIANT I:
 *
 * let v = var "" in
 * let _ = match_string [ Lazystring;
 *                        Literal "("; Record (v, [Lazystring]); Literal ")";
 *                        Anystring ]
 *                      s 
 * in found_string_of_var v
 *
 * - finds the first substring with parantheses, e.g.
 *   s = "abc(def)ghi(jkl)mno" returns "def"
 *
 * To get the last substring, swap Lazystring and Anystring at the beginning
 * resp. end.
 *
 * VARIANT II:
 *
 * let v = var "" in
 * let _ = match_string [ Lazystring;
 *                        Literal "("; Record (v, [Anystring]); Literal ")";
 *                        Anystring ]
 *                      s 
 * in found_string_of_var v
 *
 * - for s = "abc(def)ghi(jkl)mno" it is returned "def)ghi(jkl"
 *)

(* ---------------------------------------------------------------------- *)

(* EXAMPLE:
 *
 * let v = var "" in
 * let digits = mkset "0-9" in
 * let digits_re = [ Record(v, [ Anychar_from digits;  Anystring_from digits])]
 * in
 * replace_matched_substrings digits_re [ ReplaceLiteral "D" ] [] "ab012cd456fg"
 *
 * yields: ("abDcdDfg", 2)
 *
 * VARIANT I: 
 *
 * replace_matched_substrings digits_re [ ReplaceLiteral "D" ] 
 *                                      [ Limit 1 ] "ab012cd456fg"
 *
 * yields: ("abDcd456fg", 1)
 * 
 * VARIANT II:
 * 
 * replace_matched_substrings digits_re [ ReplaceLiteral "D" ] 
 *                                      [ Anchored ] "ab012cd456fg"
 *
 * yields: ("ab012cd456fg", 0)
 *
 * VARIANT III:
 * 
 * replace_matched_substrings digits_re [ ReplaceLiteral "D" ] 
 *                                      [ Anchored ] "012"
 *
 * yields: ("D", 1)
 *
 * VARIANT IV:
 * 
 * let f() = string_of_int(1+int_of_string(string_of_var v)) in
 * replace_matched_substrings digits_re [ ReplaceFunction f ] 
 *                                      [] "ab012cd456fg"
 *
 * yields: ("ab13cd457fg", 2)
 *)


(* ======================================================================
 * History:
 * 
 * $Log: xstr_match.mli,v $
 * Revision 1.2  1999/07/04 20:02:08  gerd
 * 	Added Lazystring, Lazystring_from.
 * 	Added replace_matched_substring function.
 * 	Changed the structure of 'variable'. 'sref' is either an arbitrary
 * string, or it is the input string of the matching function. 'from' and
 * 'len' are always used.
 *
 * Revision 1.1  1999/06/27 23:03:38  gerd
 * 	Initial revision.
 *
 * 
 *)
