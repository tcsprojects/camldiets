module type MeasurableType =
  sig
    type t
    val compare : t -> t -> int
	val pred : t -> t
	val succ : t -> t
	val dist : t -> t -> int
  end

module Make (Ord : MeasurableType) : Set.S with type elt = Ord.t

module MeasurableInt : MeasurableType with type t = int

