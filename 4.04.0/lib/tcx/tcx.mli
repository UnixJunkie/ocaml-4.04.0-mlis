(*
 * Author: Anton Yabchinskiy
 * License: MIT (see LICENSE file for details)
 *)

module Position :
sig
  type t = {
      latitude : float;       (* [-90, 90] *)
      longitude : float;      (* [-180, 180] *)
    }
end

module Date :
sig
  type t = {
      year : int;
      month : int;
      day : int;
    }
end

module Time :
sig
  type t = {
      hour : int;
      minute : int;
      second : int;
    }
end

module Time_zone :
sig
  type t = {
      hours : int;            (* [-12, 14] *)
      minutes : int;          (* [0, 59] *)
    }

  val utc : t
end

module Timestamp :
sig
  type t = {
      date : Date.t;
      time : Time.t;
      time_zone : Time_zone.t option;
    }

  val of_unix_time : float -> t

  val to_unix_time : t -> float
end

module Sensor_state :
sig
  type t = Present | Absent
end

module Intensity :
sig
  type t = Active | Resting
end

module Trigger_method :
sig
  type t = Manual | Distance | Location | Time | Heart_rate
end

module Sport :
sig
  type t = Running | Biking | Other
end

module Build_type :
sig
  type t = Internal | Alpha | Beta | Release
end

module Version :
sig
  type t = {
      major : int;
      minor : int;
      build_major : int option;
      build_minor : int option;
    }
end

module Device :
sig
  type t = {
      name : string;
      unit_id : int64;
      product_id : int;
      version : Version.t;
    }
end

module Build :
sig
  type t = {
      version : Version.t;
      build_type : Build_type.t option;
      time : string option;
      builder : string option;
    }
end

module Lang_id :
sig
  type t = string
end

module Part_number :
sig
  type t = string * string * string
end

module Application :
sig
  type t = {
      name : string;
      build : Build.t;
      lang_id : Lang_id.t;
      part_number : Part_number.t;
    }
end

module Source :
sig
  type t = Device of Device.t | Application of Application.t
end

module Track_point :
sig
  type t = {
      time : Timestamp.t;
      position : Position.t option;
      altitude : float option; (* m *)
      distance : float option; (* m *)
      heart_rate : int option; (* bpm *)
      cadence : int option;    (* rpm *)
      sensor_state : Sensor_state.t option;
    }

  val empty : t
end

module Track :
sig
  type t = {
      points : Track_point.t List_ext.Non_empty.t;
    }
end

module Activity_lap :
sig
  type t = {
      start_time : Timestamp.t;
      total_time : float;     (* s *)
      distance : float;       (* m *)
      maximum_speed : float option; (* m/s *)
      calories : int;               (* kcal *)
      average_heart_rate : int option; (* bpm *)
      maximum_heart_rate : int option; (* bpm *)
      intensity : Intensity.t;
      cadence : int option;   (* rpm *)
      trigger_method : Trigger_method.t;
      tracks : Track.t list;
      notes : string option;
    }

  val empty : t
end

module Activity :
sig
  type t = {
      id : Timestamp.t;
      sport : Sport.t;
      laps : Activity_lap.t List_ext.Non_empty.t;
      notes : string option;
      creator : Source.t option;
    }

  val empty : t
end

type t = {
    activities : Activity.t list;
    author : Source.t option;
  }

val of_string : string -> t

val to_string : t -> string

val parse_file : string -> t

val format_file : t -> string -> unit

module Iter :
sig
  type t = [`Activity of Activity.t |
            `Activity_lap of Activity_lap.t |
            `Track of Track.t |
            `Track_point of Track_point.t]
end

val fold : ('a -> Iter.t -> 'a) -> 'a -> t -> 'a

val iter : (Iter.t -> unit) -> t -> unit

(** Function [f] must return the same [Iter] tag, or [Failure] will be
 * raised. *)
val map : (Iter.t -> Iter.t) -> t -> t
