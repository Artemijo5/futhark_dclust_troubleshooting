-- Functions for finding the maximum connected subgraphs of an undirected graph,
-- assuming edges are represented by their nodes' index pairs,
-- (i1,i2), i1<i2.

-- BFS is performed through a series of successive reduce_by_index calls.
-- In each iteration, each point looks for the smallest index any of its neighbours have found,
-- until convergence.



-- | Assign the same id to points belonging to the same connected subgraph.
-- The undirected graph is represented as an array of unique index pairs (i1,i2), i1<=i2.
-- A subraph's id is the smallest index of its elements.
--
-- The algorithm used is a parallel Breadth-First Traversal.
--
-- Parameters:
-- k : number of nodes (== max node id + 1)
-- pairs : undirected graph
-- Returns:
-- a k-sized array with the subgraph id of each node
def get_connected_subgraph_ids
	(k : i64)
	(pairs : [](i64,i64))
: [k]i64 =
	let (mins,maxs) = unzip pairs
	let (_,g_ids) =
	-- In each iteration, node k asks itself & its neihbours
	-- for the minimum-indexed node they currently 'see',
	-- until convergence.
	--
	-- Worst case O(k) span, O(k^2) work
	-- (assuming reduce_by_inddex has O(1) span, O(k) work)
	-- if the entire graph is a line with only 2 edges per node excluding the extremes.
	--
	-- In general, span = O(length of the largest path to the min node in any connected subgraph).
	loop (old_mins, new_mins) = (replicate k (-1), iota k)
	while (any (id) (map2 (!=) old_mins new_mins)) do
		let mins_from_mins = mins |> map (\i -> new_mins[i])
		let mins_from_maxs = maxs |> map (\i -> new_mins[i])
		let pivots_from_maxs = reduce_by_index (copy new_mins)
			(i64.min) i64.highest mins mins_from_maxs
		let pivots_from_mins = reduce_by_index pivots_from_maxs
			(i64.min) i64.highest maxs mins_from_mins
		in (new_mins, pivots_from_mins)
	in g_ids

import "basics"

-- | Apply dictionary encoding to subgraph id's.
def encode_subgraph_ids [k] (sg_ids : [k]i64) : [k]i64 =
	let flags = iota k |> map2 (==) sg_ids
	let flag_ids = flags |> dict_encoding
	in sg_ids |> map (\i -> flag_ids[i])