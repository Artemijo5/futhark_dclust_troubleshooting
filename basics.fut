-- Basic routines

-- | Exclusive scan operation (from Futhark by Example).
def exscan f ne xs =
	map2
		(\i x -> if i==0 then ne else x)
		(indices xs)
		(rotate (-1) (scan f ne xs))

-- | Sequential map function, for some cases of small static parallelism.
def seqmap [n] 't 'ot
	(dummy_out : ot)
	(f : t -> ot)
	(xs: [n]t)
: [n]ot =
	if n==0 then ([] :> [n]ot) else
	loop buff = (replicate n dummy_out) for j in (0..<n) do
		buff with [j] = f xs[j]

-- | Function to identify the group boundaries in an array of grouped keys.
-- Returns a boolean array, with the first index of each group being true.
-- NOTE : the previous element is on the left side of the neq comparator.
def group_boundaries [n] 't (neq : t -> t -> bool) (xs : [n]t)
: [n]bool = iota n
	|> map (\i -> if i==0 then true else (xs[i-1] `neq` xs[i]))

-- | Dictionary encoding: assign compact i64 ids to grouped keys, using the group boundaries.
def dict_encoding [n] (gbs : [n]bool)
: [n]i64 = gbs
	|> map (i64.bool)
	|> scan (+) 0
	|> map (\i -> i-1)

import "lib/github.com/diku-dk/sorts/radix_sort"

-- | Radix-based bucket-sort for key-value pairs.
-- Meant for a 'small' number of compactly numbered buckets.
def bucket_sort [n] 't
	(_: i32)
	(num_buckets : i64)
	(ks : [n]i64)
	(xs : [n]t)
=
	let msb = num_buckets - 1 |> i64.clz |> (i32.-) i64.num_bits
	in zip ks xs
    	|> radix_sort msb (\i (k,_) -> i64.get_bit i k)
		|> unzip

import "lib/github.com/athas/vector/vector"

-- | Module type for distance computations.
module type distance = {
	type t
	type vector 'a

	-- | Obtain the distance between 2 points.
	val dist : vector t -> vector t -> t

	-- | Check if 2 points meet a neighbourhood criterion.
	val check_neighbourhood : t -> vector t -> vector t -> bool
}

-- | Module for Euclidean distance computations.
module euclidean_dist (V : vector) (F : real)
: distance with t = F.t with vector 'a = V.vector a = {
	type t = F.t
	type vector 'a = V.vector a

	local def minus = (F.-)
	local def plus  = (F.+)
	local def times = (F.*)
	local def sqrt  = (F.sqrt)

	local def zero = F.i32 0

	local def leq = (F.<=)

	def dist pt1 pt2 = V.map2 (minus) pt1 pt2
		|> V.map (\x -> x `times` x)
		|> V.reduce (plus) zero
		|> sqrt

	def check_neighbourhood eps pt1 pt2 =
		(dist pt1 pt2) `leq` eps
}

-- | Bulk binary search to locate the last matching element.
--
-- If no match exists, outputs index of largest element smaller than v.
-- If no smaller element exists, outputs (-1).
-- Also outputs (-1) if the initial index < 0.
--
-- Note: vs are on the left side of all comparisons.
def bsearch_last [nvs] [n] 't
	(geq: t -> t -> bool)
	(lt : t -> t -> bool)
	(min_is : [nvs]i64)
	(max_is : [nvs]i64) -- exclusive
	(xs : [n]t)
	(vs : [nvs]t)
: [nvs]i64 = vs |> map3 (\i_min i_max v ->
	let (found_at,_) = loop (i, last_step) = (i_min, i_max-i_min)
	while i>=0 && i>=i_min && i<i_max &&
		!( (v `geq` xs[i]) && ( i==(i_max-1) || (v `lt` xs[i+1]) ) )
	do
		-- check for kv>=cv && kv<nv is done in loop conditions
		-- so inside loop assume that isn't the case
		let this_step = (last_step+1)/2 in
		if (v `lt` xs[i]) then
			(i64.max i_min (i-this_step), this_step)
		else
			(i64.min (i_max-1) (i+this_step), this_step)
	in found_at
) min_is max_is