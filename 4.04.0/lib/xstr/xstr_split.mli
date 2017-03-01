(* $Id: xstr_split.mli,v 1.2 1999/07/04 20:02:20 gerd Exp $
 * ----------------------------------------------------------------------
 * Split strings into words
 *)

val split_string :
    string -> bool -> bool -> string list -> string -> string list

  (* split_string ignoreset ignoreleft ignoreright separators s:
   *
   * Splits 's' into words; the other parameters control the recognition
   * of words.
   * 'separators' is a list of strings that may separate the words.
   * 'ignoreset' is a list of characters (written as string) that are
   *    ignored before and after separators.
   * 'ignoreleft' controls whether characters can be ignored before the
   *    first word, too.
   * 'ignoreright' controls whether characters can be ignored after the
   *    last word, too.
   *
   * Empty separators may have strange effects. Do not use them.
   * 
   * 'ignoreleft = false' implies that at least one word will be found.
   * For example:
   *    split_string " " false true [ "," ] "   " = [ "" ]
   * 'ignoreright = false' does not have this effect:
   *    split_string " " true false [ "," ] "   " = [ ]
   *
   * Note that it is possible to ignore characters that are the beginning
   * of separators. E.g.
   *    split_string " " true true [ " " ] " ab c d " = [ "ab"; "c"; "d" ]
   *
   * If many strings are splitted with the same rules, the following is
   * the recommended way:
   *     let split_rule = split_string ign il ir seps in
   *     ...call many times split_rule s for different values of s...
   * It is much more efficient than calling split_string directly.
   *
   * [Note thread-safety: such a 'split_rule' is allowed to be shared by
   *  several threads.]
   *
   * EXAMPLES:
   *
   * - split_string " " true true [ "," ] "a, b, c ,d " 
   *   = [ "a"; "b"; "c"; "d" ]
   * - split_string "" true true [ "," ] "a, b, c ,d " 
   *   = [ "a"; " b"; " c "; "d " ]
   * - split_string " " false false [ "," ] "a, b, c ,d " 
   *   = [ "a"; "b"; "c"; "d " ]
   *)
   

(* ======================================================================
 * History:
 * 
 * $Log: xstr_split.mli,v $
 * Revision 1.2  1999/07/04 20:02:20  gerd
 * 	Improved the comments.
 *
 * Revision 1.1  1999/06/27 23:03:38  gerd
 * 	Initial revision.
 *
 * 
 *)
