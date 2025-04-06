module type MeasurableType =
  sig
    type t
    val compare : t -> t -> int
	  val pred : t -> t
	  val succ : t -> t
	  val dist : t -> t -> int
  end

module type DietSet = sig
  include Set.S
  val cardinal: t -> int
  val height: t -> int
end
  
module Make (Ord : MeasurableType) : DietSet with type elt = Ord.t

module MeasurableInt : MeasurableType with type t = int

