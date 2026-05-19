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
def get_connected_subgraph_ids [n]
	(k : i64)
	(pairs : [n](i64,i64))
: [k]i64 =
	let (mins,maxs) = unzip pairs
	let (_,g_ids,_) =
	-- In each iteration, node k asks itself & its neihbours
	-- for the minimum-indexed node they currently 'see',
	-- until convergence.
	--
	-- Worst case O(k) span, O(k^2) work
	-- (assuming reduce_by_inddex has O(1) span, O(k) work)
	-- if the entire graph is a line with only 2 edges per node excluding the extremes.
	--
	-- In general, span = O(length of the largest path to the min node in any connected subgraph).
	loop (old_mins, new_mins, iter) = (replicate k (-1), iota k, 0)
	while ((any (id) (map2 (!=) old_mins new_mins))) do
		let mins_from_mins = mins |> map (\i -> new_mins[i])
		let mins_from_maxs = maxs |> map (\i -> new_mins[i])
		let pivots_from_maxs = hist (i64.min) i64.highest k
			mins mins_from_maxs
		let pivots_from_mins = hist (i64.min) i64.highest k
			maxs mins_from_mins
		let pivots_final = map3 (\p1 p2 p3 -> i64.min (i64.min p1 p2) p3)
			pivots_from_mins pivots_from_maxs new_mins
		in (new_mins, pivots_final,iter+1)
	in g_ids

import "basics"

-- | Wrapper for get_connected_subgraph_ids that first makes pairs unique.
def get_connected_subgraph_ids_from_unique
	(k : i64)
	(pairs : [](i64,i64))
: [k]i64 =
	let num_buckets = 1 + (pairs |> map (.1) |> i64.maximum)
	let pairs1 = pairs
		|> bucket_sort 2 num_buckets (pairs |> map (.1))
		|> (.1)
	let pairs2 = pairs1
		|> bucket_sort 2 num_buckets (pairs1 |> map (.0))
		|> (.1)
	let pairs_unique = pairs2
		|> group_boundaries (\(x1,y1) (x2,y2) -> x1!=x2 || y1!=y2)
		|> zip pairs2
		|> filter (.1)
		|> map (.0)
	in get_connected_subgraph_ids k pairs_unique

-- | Apply dictionary encoding to subgraph id's.
def encode_subgraph_ids [k] (sg_ids : [k]i64) : [k]i64 =
	let flags = iota k |> map2 (==) sg_ids
	let flag_ids = flags |> dict_encoding
	in sg_ids |> map (\i -> flag_ids[i])