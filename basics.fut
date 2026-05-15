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

-- | Module for 2-dimensional vectors.
module vector_2 = cat_vector vector_1 vector_1

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

-- | Euclidean distance module for 2d points of f64 values.
module eucl2_f64 = euclidean_dist vector_2 f64