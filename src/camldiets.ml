(***********************************************************
 *                                                         *
 * The code for handling the AVL trees is borrowed from    *
 * the Objective Caml Standard Library Set module.         *
 *                                                         *
 * (c) Xavier Leroy, projet Cristal, INRIA Rocquencourt    *
 *                                                         *
 ***********************************************************)


 module AvlTree = struct

	let bal_const = 1

	type 'a avltree = Empty | Node of 'a * int * 'a avltree * 'a avltree

	let empty = Empty

	let is_empty = function Empty -> true | _ -> false

	let singleton x = Node (x, 1, Empty, Empty)

	(*
	let left = function
	    Empty -> raise Not_found
	|   Node (_, _, l, _) -> l

	let right = function
	    Empty -> raise Not_found
	|	Node (_, _, _, r) -> r
	*)

	let rec min_elt = function
		Empty -> raise Not_found
	|	Node (x,_,Empty,_) -> x
	|	Node (_,_,left,_) -> min_elt left

	let rec max_elt = function
		Empty -> raise Not_found
	|	Node (x,_,_,Empty) -> x
	|	Node (_,_,_,right) -> max_elt right

	(*
	let root = function
	    Empty -> raise Not_found
	|	Node (v, _, _, _) -> v
	*)

	let height = function
	    Empty -> 0
	|	Node (_, h, _, _) -> h

	let height_join left right =
		1 + max (height left) (height right)

	let create x l r =
		Node (x, height_join l r, l, r)

	let balance x l r =
	    let hl = height l in
	    let hr = height r in
	    if hl > hr + bal_const then (
	    	match l with
	      		Empty -> invalid_arg "AvlTree.balance"
	        |	Node(lvx, _, ll, lr) ->
	          		if height ll >= height lr then
	            		create lvx ll (create x lr r)
	            	else (
			            match lr with
	        		    	Empty -> invalid_arg "AvlTree.balance"
	        		    |	Node(lrx, _, lrl, lrr)->
	                		create lrx (create lvx ll lrl) (create x lrr r)
	                )
	    )
	    else if hr > hl + bal_const then (
	    	match r with
	        	Empty -> invalid_arg "AvlTree.balance"
	        |	Node(rvx, _, rl, rr) ->
	        		if height rr >= height rl then
	            		create rvx (create x l rl) rr
	          		else (
	            		match rl with
	              			Empty -> invalid_arg "AvlTree.balance"
	            		|	Node(rlx, _, rll, rlr) ->
	                			create rlx (create x l rll) (create rvx rlr rr)
	         		)
	    )
	    else Node(x, (if hl >= hr then hl + 1 else hr + 1), l, r)

	let rec join v l r =
	    let rec myadd left x = function
	        Empty -> Node(x, 1, Empty, Empty)
	    |   Node(vx, _, l, r) ->
	      		if left then balance vx (myadd left x l) r
	      		        else balance vx l (myadd left x r)
	    in
	    match (l, r) with
	    	(Empty, _) -> myadd true v r
	    |	(_, Empty) -> myadd false v l
	    |	(Node(lx, lh, ll, lr), Node(rx, rh, rl, rr)) ->
		        if lh > rh + bal_const then balance lx ll (join v lr r) else
		        if rh > lh + bal_const then balance rx (join v l rl) rr else
		        create v l r

	let rec take_min = function
		Empty -> raise Not_found
	|	Node (x, _, Empty, right) -> (x, right)
	|	Node (x, _, left, right) -> let (x', left') = take_min left in (x', join x left' right)

	let rec take_max = function
		Empty -> raise Not_found
	|	Node (x, _, left, Empty) -> (x, left)
	|	Node (x, _, left, right) -> let (x', right') = take_max right in (x', join x left right')

	let reroot l r =
        if height l > height r
        then let (i, l') = take_max l in join i l' r
        else if r = Empty then Empty
        else let (i, r') = take_min r in join i l r'

	let rec take_min_iter = function
		Empty -> raise Not_found
	|	Node (x, _, Empty, right) -> (x, right)
	|	Node (x, _, Node(a, _, left, mid), right) ->
			let n = Node (x, height_join mid right, mid, right) in
			take_min_iter (Node (a, height_join left n, left, n))

	let take_min_iter2 = function
		Empty -> (None, Empty)
	|	t -> let (i, s) = take_min_iter t in (Some i, s)

	let rec take_max_iter = function
		Empty -> raise Not_found
	|	Node (x, _, left, Empty) -> (x, left)
	|	Node (x, _, left, Node(a, _, mid, right)) ->
			let n =  Node (x, height_join left mid, left, mid) in
			take_max_iter (Node (a, height_join n right, n, right))

	let take_max_iter2 = function
		Empty -> (None, Empty)
	|	t -> let (i, s) = take_max_iter t in (Some i, s)

	let iter f t =
		let rec iter_aux = function
			(None, _) -> ()
		|	(Some x, rest) -> (
				f x;
				iter_aux (take_min_iter2 rest)
			)
		in
			iter_aux (take_min_iter2 t)

	let fold f t a =
		let rec fold_aux a = function
			(None, _) -> a
		|	(Some x, rest) -> fold_aux (f x a) (take_min_iter2 rest)
	  	in
			fold_aux a (take_min_iter2 t)

	let fold_right f t a =
		let rec fold_aux a = function
			(None, _) -> a
		|	(Some x, rest) -> fold_aux (f x a) (take_max_iter2 rest)
	  	in
			fold_aux a (take_max_iter2 t)

	(*
	let elements t  = fold_right (fun x -> fun xs -> x::xs) t []

	let for_all f t = fold (fun x -> fun y -> (f x) && y) t true

	let exists  f t = fold (fun x -> fun y -> (f x) || y) t false

	let cardinal t = fold (fun _ a -> a + 1) t 0
	*)

	let choose = function
  		Empty -> raise Not_found
    |	Node (x,_,_,_) -> x

end


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


module Make(Ord: MeasurableType) =
  struct
    type elt = Ord.t

    type t = (elt * elt) AvlTree.avltree

	let safe_pred limit x =
  		if Ord.compare limit x < 0 then Ord.pred x else x

	(*
	let safe_succ limit x =
		if Ord.compare limit x > 0 then Ord.succ x else x
	*)

	let max x y =
		if Ord.compare x y > 0 then x else y

	let min x y =
		if Ord.compare x y < 0 then x else y

	let height t =
    	AvlTree.height t

	let find_del_left p =
		let rec find = function
			AvlTree.Empty -> (p,AvlTree.Empty)
	    |	AvlTree.Node ((x,y),_,left,right) ->
				if Ord.compare p (Ord.succ y) > 0 then
				    let (p', right') = find right in
				    (p', AvlTree.join (x, y) left right')
				else if Ord.compare p x < 0 then find left
				else (x, left)
		in
			find

	let find_del_right p =
		let rec find = function
			AvlTree.Empty -> (p,AvlTree.Empty)
	    |	AvlTree.Node ((x,y),_,left,right) ->
				if Ord.compare p (Ord.pred x) < 0 then
				    let (p', left') = find left in
				    (p', AvlTree.join (x, y) left' right)
				else if Ord.compare p y > 0 then find right
				else (y, right)
		in
			find

	let empty = AvlTree.empty

  	let is_empty = AvlTree.is_empty

	let rec mem z = function
		AvlTree.Empty -> false
	|	AvlTree.Node ((x, y), _, left, right) ->
			if Ord.compare z x < 0 then mem z left
			else if Ord.compare z y > 0 then mem z right
			else true

	let min_elt t =
		fst (AvlTree.min_elt t)

	let max_elt t =
		snd (AvlTree.max_elt t)

	let rec add p = function
		AvlTree.Empty -> AvlTree.Node((p, p), 1, AvlTree.Empty, AvlTree.Empty)
	|	(AvlTree.Node((x, y), h, left, right)) as t ->
			if Ord.compare p x >= 0
			then if Ord.compare p y <= 0 then t
				 else if Ord.compare p (Ord.succ y) > 0
				 then AvlTree.join (x, y) left (add p right)
				 else if right = AvlTree.Empty
				 then AvlTree.Node ((x, p), h, left, right)
				 else let ((u, v), r) = AvlTree.take_min right in
					  if Ord.pred u = p
					  then AvlTree.join (x, v) left r
					  else AvlTree.Node ((x, p), h, left, right)
			else if Ord.compare p (Ord.pred x) < 0
			then AvlTree.join (x, y) (add p left) right
			else if left = AvlTree.Empty
			then AvlTree.Node ((p, y), h, left, right)
			else let ((u, v), l) = AvlTree.take_max left in
				 if Ord.succ v = p
				 then AvlTree.join (u, y) l right
				 else AvlTree.Node ((p, y), h, left, right)

    let of_list = List.fold_left (fun x y -> add y x) empty

	let rec insert (p, q) = function
		AvlTree.Empty -> AvlTree.Node((p, q), 1, AvlTree.Empty, AvlTree.Empty)
	|	AvlTree.Node((x, y), _, left, right) ->
			if Ord.compare q (Ord.pred x) < 0
			then AvlTree.join (x, y) (insert (p,q) left) right
			else if Ord.compare p (Ord.succ y) > 0
			then AvlTree.join (x, y) left (insert (p,q) right)
			else let (x',left') = if Ord.compare p x >= 0 then (x,left) else find_del_left p left in
				 let (y',right') = if Ord.compare q y <= 0 then (y,right) else find_del_right q right in
				 AvlTree.join (x', y') left' right'

	let singleton x = AvlTree.singleton (x,x)

	let rec remove z = function
		AvlTree.Empty -> AvlTree.Empty
	|	AvlTree.Node ((x,y),h,left,right) ->
			 let czx = Ord.compare z x in
			 if czx < 0 then AvlTree.join (x, y) (remove z left) right
			 else let cyz = Ord.compare y z in
				  if cyz < 0 then AvlTree.join (x, y) left (remove z right)
				  else if cyz = 0
					   then if czx = 0 then AvlTree.reroot left right
							else AvlTree.Node ((x, Ord.pred y), h, left, right)
					   else if czx = 0 then AvlTree.Node ((Ord.succ x, y), h, left, right)
							else insert (Ord.succ z, y) (AvlTree.Node ((x, Ord.pred z), h, left, right))

	let rec union input stream =
	  	let rec union' input limit head stream =
	  		match head with
	  		    None -> (input, None, AvlTree.Empty)
	  		|   Some (x, _) ->
	  				match input with
	  					AvlTree.Empty -> (AvlTree.Empty, head, stream)
	  				|	AvlTree.Node ((a, b), _, left, right) ->
	  						let (left', head, stream) = if Ord.compare x a < 0 then union' left (Some (Ord.pred a)) head stream
	  						                                           else (left, head, stream)
	  						in
	  							union_helper left' (a, b) right limit head stream
		and union_helper left (a, b) right limit head stream =
	  		match head with
	  		    None -> (AvlTree.join (a,b) left right, None, AvlTree.Empty)
	  		|   Some (x, y) ->
	  				let greater_limit z =
	  					match limit with
	  						None -> false
	  					|	Some u -> Ord.compare z u >= 0
	  				in
	  				if (Ord.compare y a < 0) && (Ord.compare y (Ord.pred a) < 0)
	  				then let left' = insert (x, y) left in
	  				     let (head, stream) = AvlTree.take_min_iter2 stream in
	  				     union_helper left' (a, b) right limit head stream
	  				else if (Ord.compare x b > 0) && (Ord.compare x (Ord.succ b) > 0)
	  				then let (right', head, stream) = union' right limit head stream in
	  				     (AvlTree.join (a,b) left right', head, stream)
	  				else if Ord.compare b y >= 0
	  				then let (head, stream) = AvlTree.take_min_iter2 stream in
	  					 union_helper left (min a x, b) right limit head stream
	  				else if greater_limit y
	  				then (left, Some (min a x, y), stream)
	  				else let (right', head, stream) = union' right limit (Some (min a x, y)) stream in
	  				     (AvlTree.reroot left right', head, stream)
	 	in
	 		if AvlTree.height stream > AvlTree.height input then union stream input
	 		else let (head, stream) = AvlTree.take_min_iter2 stream in
	             let (result, head, stream) = union' input None head stream in
	             match head with
	             	None -> result
	             |	Some i -> AvlTree.join i result stream

  	let iter f t =
		let g (x, y) =
			let z = ref x in
			while Ord.compare !z y < 0 do
				f !z;
				z := Ord.succ !z;
			done;
			f !z
		in
			AvlTree.iter g t


	let fold f t a =
		let rec g (x, y) a =
		  if Ord.compare x y < 0
		  then g (Ord.succ x, y) (f x a)
		  else f x a
		in
			AvlTree.fold g t a

	let fold_right f t a =
		let rec g (x, y) a =
		  if Ord.compare x y < 0
		  then g (x, Ord.pred y) (f y a)
		  else f y a
		in
			AvlTree.fold_right g t a

    let find x t =
        match fold (fun y a -> if Ord.compare x y = 0 then Some y else a) t None with
        |   Some y -> y
        |   None -> raise Not_found

    let map f t =
        fold (fun x -> add (f x)) t empty

	let elements t  =
  		fold_right (fun x -> fun xs -> x::xs) t []

	let for_all f t =
		fold (fun x -> fun y -> (f x) && y) t true

	let exists f t =
		fold (fun x -> fun y -> (f x) || y) t false

	let rec filter f = function
		AvlTree.Empty -> AvlTree.Empty
	|	AvlTree.Node ((x,y),_,left,right) ->
			let z = ref y in
			let z' = ref y in
			let s = ref y in
			let ivs = ref [] in
			let good = ref false in
			while Ord.compare x !z < 0 do
				if (not !good) && (f !z) then (
					good := true;
					s := !z
				)
				else if !good && (not (f !z)) then (
					good := false;
					ivs := (!z', !s)::!ivs
				);
				z' := !z;
				z := Ord.pred !z
			done;
			if (f !z)
			then ivs := (if !good then (!z, !s) else (!z, !z))::!ivs
			else if !good then ivs := (!z', !s)::!ivs;
			match !ivs with
				[] -> AvlTree.reroot (filter f left) (filter f right)
			|   i::is -> List.fold_left (fun x y -> (insert y x)) (AvlTree.join i (filter f left) (filter f right)) is


	let cardinal t =
	    let rec cardinal_aux a = function
	    	[] -> a
	    |	AvlTree.Empty::ts -> cardinal_aux a ts
	    |	(AvlTree.Node ((x,y),_,left,right))::ts -> cardinal_aux (a+(Ord.dist x y)+1) (left::right::ts)
	    in
		    cardinal_aux 0 [t]

	let choose t =
  		fst (AvlTree.choose t)

	let rec split x = function
		AvlTree.Empty ->
			(AvlTree.Empty, false, AvlTree.Empty)
	|	AvlTree.Node((a, b), _, l, r) ->
			let cxa = Ord.compare x a in
			if cxa < 0
			then let (ll, pres, rl) = split x l in (ll, pres, AvlTree.join (a, b) rl r)
			else let cbx = Ord.compare b x in
				 if cbx < 0
				 then let (lr, pres, rr) = split x r in (AvlTree.join (a, b) l lr, pres, rr)
				 else ((if cxa = 0 then l else insert (a, Ord.pred x) l),
					   true,
					   (if cbx = 0 then r else insert (Ord.succ x, b) r))

	let rec inter input stream =
	  	let rec inter' input head stream =
	  		match head with	None -> (AvlTree.Empty, None, AvlTree.Empty)
	  		|   Some (x, _) -> match input with
	  				AvlTree.Empty -> (AvlTree.Empty, head, stream)
	  			|	AvlTree.Node ((a, b), _, left, right) ->
	  					let (left, head, stream) = if Ord.compare x a < 0 then inter' left head stream
	  					                           else (AvlTree.Empty, head, stream) in
	  					inter_help (a, b) right left head stream
	  	and inter_help (a, b) right left head stream =
	  		match head with	None -> (left, None, AvlTree.Empty)
	  		|	Some (x, y) ->
	  			let cya = Ord.compare y a in
	  			if cya < 0 then if stream = AvlTree.Empty then (left, None, AvlTree.Empty)
	  							else let (head, stream) = AvlTree.take_min_iter stream in
	  							     inter_help (a, b) right left (Some head) stream
	  			else let cbx = Ord.compare b x in
	                 if cbx < 0 then let (right, head, stream) = inter' right head stream in
	                                 (AvlTree.reroot left right, head, stream)
	                 else if Ord.compare y (safe_pred y b) >= 0
	                      then let (right, head, stream) = inter' right head stream in
	                           ((AvlTree.join (max x a, min y b) left right), head, stream)
	                      else let left =  (insert (max x a, y) left) in
	                           inter_help (Ord.succ y, b) right left head stream
	 	in
	 		if AvlTree.height stream > AvlTree.height input
	 		then inter stream input
	 		else if stream = AvlTree.Empty then AvlTree.Empty
	             else let (head, stream) = AvlTree.take_min_iter stream in
	                  let (result, _, _) = inter' input (Some head) stream in
	                  result

	let diff input stream =
	  	let rec diff' input head stream =
	  		match head with	None -> (input, None, AvlTree.Empty)
	  		|   Some (x, _) -> match input with
	  				AvlTree.Empty -> (AvlTree.Empty, head, stream)
	  			|	AvlTree.Node ((a, b), _, left, right) ->
	  					let (left, head, stream) = if Ord.compare x a < 0 then diff' left head stream
	  					                           else (left, head, stream) in
	  					diff_helper (a, b) right left head stream
	  	and diff_helper (a, b) right left head stream =
	  		match head with	None -> (AvlTree.join (a, b) left right, None, AvlTree.Empty)
	  		|	Some (x, y) ->
	  			let cya = Ord.compare y a in
	  			if cya < 0 then let (head, stream) = AvlTree.take_min_iter2 stream in
	        				    diff_helper (a, b) right left head stream
	  			else let cbx = Ord.compare b x in
	                 if cbx < 0 then let (right, head, stream) = diff' right head stream in
	                                 (AvlTree.join (a, b) left right, head, stream)
	                 else if Ord.compare a x < 0
	                      then diff_helper (x, b) right ((insert (a, Ord.pred x) left)) head stream
	                      else if Ord.compare y b < 0
	                           then let (head, stream) = AvlTree.take_min_iter2 stream in
	                                diff_helper (Ord.succ y, b) right left head stream
	                           else let (right, head, stream) = diff' right head stream in
	                           		(AvlTree.reroot left right, head, stream)
	 	in
	 		if stream = AvlTree.Empty then input
	        else let (head, stream) = AvlTree.take_min_iter stream in
	             let (result, _, _) = diff' input (Some head) stream in
	            result

	let rec compare t1 t2 =
		if (t1 != AvlTree.Empty) && (t2 != AvlTree.Empty)
		then let ((ix1, iy1), r1) = AvlTree.take_min_iter t1 in
			 let ((ix2, iy2), r2) = AvlTree.take_min_iter t2 in
			 let d = Ord.compare ix1 ix2 in
			 let c = if d != 0 then -d else Ord.compare iy1 iy2 in
			 if c != 0 then c else compare r1 r2
		else if t1 != AvlTree.Empty then 1
		else if t2 = AvlTree.Empty then 0 else -1

	let equal t1 t2 =
		compare t1 t2 = 0

	let subset t1 t2 =
		let rec subset' ((x1, y1), r1) ((x2, y2), r2) =
	  	     if Ord.compare x1 x2 < 0 then false
	  	     else if Ord.compare x1 y2 > 0 then if r2 = AvlTree.Empty then false
	  	                                else subset' ((x1, y1), r1) (AvlTree.take_min_iter r2)
	  	     else let upper = Ord.compare y1 y2 in
	  	     	  if upper < 0 then if r1 = AvlTree.Empty then true
	  	     	                    else subset' (AvlTree.take_min_iter r1) ((x2, y2), r2)
	  	     	  else if upper = 0 then if r1 = AvlTree.Empty || r2 = AvlTree.Empty then r1 = AvlTree.Empty
	  	     	                         else subset' (AvlTree.take_min_iter r1) (AvlTree.take_min_iter r2)
	  	     	  else false
		in
		  	if t1 = AvlTree.Empty || t2 = AvlTree.Empty then t1 = AvlTree.Empty
		  	else subset' (AvlTree.take_min_iter t1) (AvlTree.take_min_iter t2)


	let rec partition f = function
		AvlTree.Empty -> (AvlTree.Empty, AvlTree.Empty)
	|	AvlTree.Node ((x,y),_,left,right) ->
			let z = ref y in
			let z' = ref y in
			let sg = ref y in
			let sb = ref y in
			let ivsg = ref [] in
			let ivsb = ref [] in
			let good = ref false in
			let starters = ref true in
			while Ord.compare x !z < 0 do
				if (not !good) && (f !z) then (
					good := true;
					sg := !z;
					if not !starters then ivsb := (!z', !sb)::!ivsb
				)
				else if !good && (not (f !z)) then (
					good := false;
					sb := !z;
					ivsg := (!z', !sg)::!ivsg
				);
				z' := !z;
				z := Ord.pred !z;
				starters := false
			done;
			if (f !z)
			then (
				ivsg := (if !good then (!z, !sg) else (!z, !z))::!ivsg;
				if not (!good || !starters) then ivsb := (!z', !sb)::!ivsb
			)
			else (
				ivsb := (if not !good then (!z, !sb) else (!z, !z))::!ivsb;
				if !good then ivsg := (!z', !sg)::!ivsg
			);
			let (leftg, leftb) = partition f left in
			let (rightg, rightb) = partition f right in
			((
				match !ivsg with
					[] -> AvlTree.reroot leftg rightg
				|   i::is -> List.fold_left (fun x y -> (insert y x)) (AvlTree.join i leftg rightg) is
			),(
				match !ivsb with
					[] -> AvlTree.reroot leftb rightb
				|   i::is -> List.fold_left (fun x y -> (insert y x)) (AvlTree.join i leftb rightb) is
			))

	let disjoint s1 s2 = AvlTree.is_empty (inter s1 s2)

	let min_elt_opt t = try Some (min_elt t) with Not_found -> None

	let max_elt_opt t = try Some (max_elt t) with Not_found -> None

	let choose_opt t = try Some (choose t) with Not_found -> None

	let find_opt e t = try Some (find e t) with Not_found -> None

    let find_first f t =
        match fold (fun y a -> if f y then Some y else a) t None with
        |   Some y -> y
        |   None -> raise Not_found

	let find_first_opt f t = try Some (find_first f t) with Not_found -> None

	let find_last f t =
		match fold_right (fun y a -> if f y then Some y else a) t None with
		|   Some y -> y
		|   None -> raise Not_found

	let find_last_opt f t = try Some (find_last f t) with Not_found -> None

	let to_list t = elements t

	let filter_map f t = fold (fun x t -> match f x with Some y -> add y t | None -> t) t empty

	let to_seq t = List.to_seq (to_list t)

	let to_rev_seq t = List.to_seq (List.rev (to_list t))

	let to_seq_from x t = to_seq (filter (fun y -> Ord.compare x y <= 0) t)

	let add_seq s t = List.fold_left (fun x y -> add y x) t (List.of_seq s)

	let of_seq s = add_seq s empty

end


module MeasurableInt = struct
	type t = int
	let compare = compare
	let pred = pred
	let succ = succ
	let dist x y = y - x
end
