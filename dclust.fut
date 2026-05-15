import "basics"
import "spatial_index"
import "bfs"

import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/athas/vector/vector"

-- | Module to define functions for DClust-derived algorithm.
module dclust
	(V : vector)
	(F : real)
	(D : distance with t = F.t with vector 'a = V.vector a)
= {
	type t = F.t
	type vector 'a = V.vector a

	module I = grid_index V F

	local def zero = F.i32 0i32

	local def over  = (F./)
	local def times = (F.*)
	local def minus = (F.-)
	local def plus  = (F.+)

	local def slightly_bigger = times (F.f64 1.001)

	local def leq = (F.<=)

	local def to_i64 = (F.to_i64)
	local def from_i64 = (F.i64)

	local def min = F.min
	local def minimum = F.minimum
	local def maximum = F.maximum
	
	-- | Index dataset using a regular grid index,
	-- so that cell width >= eps in all dimensions.
	def partition_dataset
		(eps : t)
		(sdv : [V.length]i64) -- intended number of subdivisions per dimension
		(pts : [](vector t))
	=
		-- get min & max values per dimension
		let perDim = iota (V.length) |> map (\i -> pts |> map (V.get i))
		let mins = perDim |> seqmap zero (minimum) |> V.from_array
		let maxs = perDim |> seqmap zero (maximum) |> V.from_array
		-- value range per dimension
		let ranges = V.map2 (minus) maxs mins
		-- approx. max subdiv with cell width > eps
		let sdv_alt = V.map (\r -> r `over` (slightly_bigger eps)) ranges
		let sdv' = V.map2 (min) (sdv |> V.from_array |> V.map (from_i64)) sdv_alt
			|> V.map (to_i64)
			|> V.map2 (i64.max) (V.replicate 1i64)
			|> V.to_array
		-- return final subdivisions and index the dataset according to them
		in (sdv', I.index_dataset sdv' pts)

	-- | Get pairs of adjacent partitions
	--
	-- Omits pairs where either partition has no points in the dataset (part_sz[pid] == 0)
	--
	-- If bidir, returns both (pid1, pid2) and (pid2, pid1) for pid1 != pid2, as well as self-pairs.
	-- If !bidir, returns only (pid1, pid2) for pid1<=pid2.
	--
	-- Output pairs are sorted by .0
	def get_adj_partitions [np]
		(bidir : bool)
		(subdiv : [V.length]i64)
		(part_sz : [np]i64)
	: [](i64, i64) =
		let subdiv_v = subdiv |> V.from_array
		-- prefix product of subdivisions per dim
		let prefix_v = subdiv |> exscan (*) 1 |> V.from_array
		-- adjacent cells:
		-- those that are +-1 position in any dimension(s)
		-- can be obtained by a d-cube surrounding the cell
		let adj_cube_increments = iota (3**V.length)
			|> map (\i -> V.iota
				|> V.map (\d -> (-1) + (i/(3**d))%3)
			)
		let part_pairs = iota np
			|> filter (\i -> part_sz[i] > 0)
			-- convert pid into a vector of subdivision steps
			|> map (\cur_pid ->
				let as_vector = prefix_v
					|> V.map (\pref -> cur_pid / pref)
					|> V.map2 (\sdv pid_suffix -> pid_suffix%sdv) subdiv_v
				in (cur_pid,as_vector)
			)
			-- map each cell to its surrounding d-cube
			|> map (\(cur_pid, as_vector) -> adj_cube_increments
				|> map (V.map2 (+) as_vector)
				|> zip (replicate (3**V.length) cur_pid)
			) |> flatten
			-- filter invalid
			|> filter (\(_,vec) -> 
				let all_positive = vec |> V.map (\v -> v>=0)
					|> V.reduce (&&) true
				let none_exceeding = vec |> V.map2 (\sdv v -> v<sdv) subdiv_v
					|> V.reduce (&&) true
				in all_positive && none_exceeding
			)
			-- convert vector back to numerical pid
			|> map (\(cur_pid,vec) ->
				let neigh_pid = vec |> V.map2 (*) prefix_v
					|> V.reduce (+) 0
				in (cur_pid, neigh_pid)
			)
			-- filter out pid1 > pid2 if !bidir
			-- as well as with count==0
			|> filter (\(pid1,pid2) -> (pid1<=pid2 || bidir) && part_sz[pid1]>0 && part_sz[pid2]>0)
		in part_pairs

	-- | Get partition information
	-- 1. pid per point
	-- 2. #points per partition
	-- 3. pairs of neighbouring partitions
	-- 4. #pairs per partition
	-- 5. index of each partition's segment in 3
	def partition_information [np] [n]
		(bidir : bool)
		(subdiv : [V.length]i64)
		(part_is     : [np]i64)
		(_ : [n](vector t)) -- indexed dataset pts
	=
		-- #pts per cell
		let part_sz = indices part_is
			|> map (\i -> if i==np-1 then n else part_is[i+1])
			|> map2 (\i1 i2 -> i2 - i1) part_is
		-- cell id for every point
		let pids = indices part_sz
			|> expand (\pid -> part_sz[pid]) (\pid _ -> pid)
			|> sized np
		-- adjacent cell pairs
		let part_pairs = get_adj_partitions bidir subdiv part_sz
		-- #adjacent cells in part_pairs per cell
		let part_pairs_sz = hist (+) 0 np
			(part_pairs |> map (.0))
			(part_pairs |> map (\_ -> 1i64))
		let part_pairs_is = part_pairs_sz |> exscan (+) 0
		in (pids,part_sz,part_pairs,part_pairs_sz,part_pairs_is)

	-- | Get neighbour counts per point.
	def get_neighbour_counts [n] [np]
		(seed_count : i64)
		(eps : t)
		(pts  : [n](vector t))
		(pids : [n]i64)
		(part_pairs : [](i64,i64))
		(part_is : [np]i64)
		(part_sz : [np]i64)
		(part_pairs_is : [np]i64)
		(part_pairs_sz : [np]i64)
	: [n]i64 =
		-- initial count = 1 (every point has at least itself in its neighbourhood)
		let init_neigh_count : [n]i64 = replicate n 1i64
		let num_iter = (n + seed_count - 1) / seed_count
		let final_neigh_count = loop neigh_count = init_neigh_count
		-- iterate over dataset with a Blocked Loop
		for j<num_iter do
			let inf = j*seed_count
			let sup = i64.min n (inf + seed_count)
			-- has 1 instance of a point's index for every neighbour that point has found
			-- separated into 2 equi-sized arrays
			let (cur_mins, cur_maxs) = (inf..<sup)
				-- expand every point to its cell's adjacent cells
				|> expand (\i1 ->
						let pid1 = pids[i1]
						in part_pairs_sz[pid1]
					)
					(\i1 ind ->
						let pid1 = pids[i1]
						let pid1_pairs_i = part_pairs_is[pid1]
						let index_in_pairs = pid1_pairs_i + ind
						let pid2 = part_pairs[index_in_pairs].1
						in (i1,pid2)
					)
				-- expand every ajacent cell to its points
				|> expand (\(_,pid2) -> part_sz[pid2])
					(\(i1,pid2) ind -> (i1, part_is[pid2] + ind))
				-- filter out i1>=i2
				|> filter (\(i1,i2) -> i1<i2)
				-- check epsilon neighbourhood
				|> filter (\(i1,i2) -> D.check_neighbourhood eps pts[i1] pts[i2])
				|> unzip
			in if (length cur_mins == 0) then neigh_count else
			-- each instance of i1 in cur_mins or cur_maxs adds 1 to neigh_count[i1]
			reduce_by_index neigh_count (+) 0 cur_mins (cur_mins |> map (\_ -> 1i64))
				|> map2 (+) (hist (+) 0 n cur_maxs (cur_maxs |> map (\_ -> 1i64)))
		in final_neigh_count
}