import "basics"
import "lib/github.com/athas/vector/vector"

-- Module type for a spatial index that subdivides space into min/max partitions,
-- aka defined by their min & max values in each dimension.
--
-- A simple example is a regular grid/cells partitioning.
module type spatial_index = {
	type t
	type vector 'a

	-- | Create the index using index_spec (the first parameter).
	-- Returns a tuple containing:
	-- 1. the dataset sorted by index-based partitions.
	-- 2. partition boundaries
	-- 3. starting index of each partition in the sorted dataset
	-- 4. transformed row indices
	val index_dataset [dim] [n] : [dim]i64 -> [n](vector t)
		-> ([n](vector t), [](vector t, vector t), []i64, [n]i64)
}

-- Regular grid subdivisions.
module grid_index (V : vector) (N : numeric)
: spatial_index with t = N.t with vector 'a = V.vector a = {
	type t = N.t
	type vector 'a = V.vector a

	local def over  = (N./)
	local def times = (N.*)
	local def minus = (N.-)
	local def plus  = (N.+)

	local def to_i64 = (N.to_i64)
	local def from_i64 = (N.i64)

	local def zero = from_i64 0i64

	local def minimum = N.minimum
	local def maximum = N.maximum

	local def get_mins_ranges (xs : [](vector t)) : (vector t, vector t) =
		let perDim = iota (V.length) |> map (\i -> xs |> map (V.get i))
		let mins = perDim |> seqmap zero (minimum) |> V.from_array
		let maxs = perDim |> seqmap zero (maximum) |> V.from_array
		let ranges = V.map2 (minus) maxs mins
		in (mins, ranges)

	local def get_partition_id
		(mins : vector t)
		(ranges : vector t)
		(idx_vec : vector i64)
		(dimPrefix : vector i64)
		(x : vector t)
	: i64 = x
		|> V.map2 (\mi xi -> xi `minus` mi) mins
		|> V.map2 (\pD xi -> xi `times` (from_i64 pD)) idx_vec
		|> V.map2 (\rg xi -> xi `over` rg) ranges
		|> V.map (to_i64)
		|> V.map2 (i64.min) (idx_vec |> V.map (\pD -> pD - 1))
		|> V.map2 (*) dimPrefix
		|> V.reduce (+) 0

	local def get_partitionBoundaries
		(mins : vector t)
		(ranges : vector t)
		(idx_vec : vector i64)
		(dimPrefix : vector i64)
		(pid : i64)
	: (vector t, vector t) =
		let pid_byDim = dimPrefix
			|> V.map2 (\spec pref -> (pid / pref) % spec) idx_vec
			|> V.map (from_i64)
		let step_byDim = idx_vec
			|> V.map (from_i64)
			|> V.map2 (over) ranges
		let stepsTaken = V.map2 (times) pid_byDim step_byDim
		let part_min = V.map2 (plus) mins stepsTaken
		let part_max = V.map2 (plus) part_min step_byDim
		in (part_min, part_max)

	-- idxSpec : [V.length]i64, represents #subdivisions per dimension
	def index_dataset idxSpec xs =
		let np = idxSpec |> reduce (*) 1
		let n = length xs
		let idx_vec = V.from_array (idxSpec |> sized V.length)
		let dimPrefix = idxSpec |> exscan (*) 1
			|> sized V.length |> V.from_array
		let (mins, ranges) = xs |> get_mins_ranges
		let pids = xs |> map (get_partition_id mins ranges idx_vec dimPrefix)
		let (pids', xs', is') = xs |> indices |> zip xs
			|> bucket_sort 2 np pids
			|> (\(ps, xis) -> let (xis1,xis2) = unzip xis in (ps,xis1,xis2))
		let firstByPid = hist (i64.+) 0i64 np (pids' |> sized n) (replicate n 1i64)
			|> exscan (+) 0
		let partBounds = iota np
			|> map (get_partitionBoundaries mins ranges idx_vec dimPrefix)
		in (xs', partBounds, firstByPid, is')
}