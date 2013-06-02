(***********************************************************
 *                      CAML DIET                          *
 *                                                         *
 *                  Copyright (c) 2010                     *
 *           Distributed under the BSD license.            *
 *                                                         *
 *                   Oliver Friedmann                      *
 *              Oliver.Friedmann@gmail.com                 *
 *                 University of Munich                    *
 *                                                         *
 *                    Martin Lange                         *
 *                Martin.Lange@gmail.com                   *
 *                 University of Kassel                    *
 *                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                         *
 * The code for handling the AVL trees is borrowed from    *
 * the Objective Caml Standard Library Set module.         *
 *                                                         *
 * (c) Xavier Leroy, projet Cristal, INRIA Rocquencourt    *
 *                                                         *
 ***********************************************************)

 
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

