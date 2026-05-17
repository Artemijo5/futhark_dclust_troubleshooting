import "basics"
import "spatial_index"
import "bfs"
import "dclust"

import "lib/github.com/athas/vector/vector"

-- HOW TO USE
--
-- Parameters :
--   seed_count : i64    - #slots used to limit nested parallelism
--   subdiv     : i64    - #cells per dimension
--   eps        : f64    - epsilon parameter for DBSCAN
--   minPts     : i64    - minPts parameter for DBSCAN
--   xs         : [n]f64 - x coordinates of 2d points
--   ys         : [n]f64 - y coordinates of 2d points
--
--  1. dat  <- index_dataset_2d_f64 eps subdiv xs ys
--  2. info <- get_part_info_2d_f64 false dat
--  3. neigh_counts <- get_neighbour_counts_2d_f64 seed_count eps dat info
--  4. is_core      <- get_is_core minPts neigh_counts
--  5. cores     <- isolate_core_pts_2d_f64 is_core dat info
--  6. core_info <- get_part_core_info_2d_f64 cores dat
--  7. core_cids <- mk_clusters_2d_f64 seed_count eps info cores core_info
--  8. info_bd <- get_part_info_2d_f64 true dat
--  9. cluster_id <- assign_cluster_ids_2d_f64 seed_count eps dat info_bd cores core_info core_cids
-- 10. res <- deindex_results dat is_core cluster_id
--
-- It is recommended to sync futhark context between steps.


-- | Module for 2-dimensional vectors.
module vector_2 = cat_vector vector_1 vector_1

-- | Euclidean distance module for 2d points of f64 values.
module eucl2_f64 = euclidean_dist vector_2 f64

-- | DClust for 2d f64 points using Euclidean distance.
module dclust2_f64 = dclust vector_2 f64 eucl2_f64

local def cols_to_vectors [n] (xs : [n]f64) (ys : [n]f64)
: [n](vector_2.vector f64) = map2
	(\x y -> vector_2.replicate x |> vector_2.set 1 y)
	xs ys

local def vectors_to_cols [n] (pts : [n](vector_2.vector f64))
: ([n]f64, [n]f64) =
	let xs = pts |> map (vector_2.get 0)
	let ys = pts |> map (vector_2.get 1)
	in (xs,ys)

type~ indexed_data_2d_f64 [n] = {
	xs : [n]f64,
	ys : [n]f64,
	subdiv : [vector_2.length]i64,
	part_is : []i64,
	og_is : [n]i64
}

entry index_dataset_2d_f64 [n]
	(eps : f64)
	(subdiv : i64)
	(xs : [n]f64)
	(ys : [n]f64)
: indexed_data_2d_f64 [n] =
	let pts = cols_to_vectors xs ys
	let (subdiv', (pts',_,part_is,og_is))
	= dclust2_f64.partition_dataset eps (replicate vector_2.length subdiv) pts
	let (xs',ys') = vectors_to_cols pts'
	in {
		xs = xs',
		ys = ys',
		subdiv = subdiv',
		part_is = part_is,
		og_is = og_is
	}

type~ partition_info_2d_f64 [n] = {
	num_parts : i64,
	partition_pairs_0 : []i64,
	partition_pairs_1 : []i64,
	part_sz : []i64,
	part_pairs_is : []i64,
	part_pairs_sz : []i64,
	pids : [n]i64
}

entry get_part_info_2d_f64 [n]
	(bidir : bool)
	(dat : indexed_data_2d_f64 [n])
: partition_info_2d_f64 [n] =
	let pts = cols_to_vectors dat.xs dat.ys
	let (pids,part_sz,part_pairs,part_pairs_sz,part_pairs_is)
	= dclust2_f64.get_partition_info bidir dat.subdiv dat.part_is pts
	in {
		num_parts = length dat.part_is,
		partition_pairs_0 = part_pairs |> map (.0),
		partition_pairs_1 = part_pairs |> map (.1),
		part_sz = part_sz,
		part_pairs_is = part_pairs_is,
		part_pairs_sz = part_pairs_sz,
		pids = pids
	}

entry get_neighbour_counts_2d_f64 [n]
	(seed_count : i64)
	(eps : f64)
	(dat  : indexed_data_2d_f64 [n])
	(info : partition_info_2d_f64 [n])
: [n]i64 =
	let pts = cols_to_vectors dat.xs dat.ys
	let part_pairs = indices info.partition_pairs_0
		|> map (\i ->
			(info.partition_pairs_0[i], info.partition_pairs_1[i])
		)
	in dclust2_f64.get_neighbour_counts
		seed_count
		eps
		pts
		info.pids
		part_pairs
		(dat.part_is  |> sized info.num_parts)
		(info.part_sz |> sized info.num_parts)
		(info.part_pairs_is |> sized info.num_parts)
		(info.part_pairs_sz |> sized info.num_parts)

entry get_is_core_2d_f64 [n]
	(minPts : i64)
	(neigh_counts : [n]i64)
: [n]bool = neigh_counts |> map (\nc -> nc>=minPts)

type~ isolated_core_pts_2d_f64 = {
	num_cores : i64,
	core_xs : []f64,
	core_ys : []f64,
	core_pids : []i64,
	core_is : []i64,
	non_core_is : []i64
}

entry isolate_core_pts_2d_f64 [n]
	(is_core : [n]bool)
	(dat  : indexed_data_2d_f64 [n])
	(info : partition_info_2d_f64 [n])
: isolated_core_pts_2d_f64 =
	let (cores,non_cores) = iota n
		|> partition (\i -> is_core[i])
	let core_xs = cores |> map (\i -> dat.xs[i])
	let core_ys = cores |> map (\i -> dat.ys[i])
	let core_pids = cores |> map (\i -> info.pids[i])
	in {
		num_cores = length cores,
		core_xs = core_xs,
		core_ys = core_ys,
		core_pids = core_pids,
		core_is = cores,
		non_core_is = non_cores
	}

type~ part_core_info_2d_f64 = {
	part_core_sz : []i64,
	part_core_is : []i64
}

entry get_part_core_info_2d_f64 [n]
	(cores : isolated_core_pts_2d_f64)
	(dat   : indexed_data_2d_f64 [n])
: part_core_info_2d_f64 =
	let (part_core_sz, part_core_is) = dclust2_f64.get_part_core_info
		dat.part_is
		cores.core_pids
	in {part_core_sz = part_core_sz, part_core_is = part_core_is}

entry mk_clusters_2d_f64 [n]
	(seed_count : i64)
	(eps : f64)
	(info  : partition_info_2d_f64 [n])
	(cores : isolated_core_pts_2d_f64)
	(core_info : part_core_info_2d_f64)
: []i64 =
	let core_pts = cols_to_vectors
		(cores.core_xs |> sized cores.num_cores)
		(cores.core_ys |> sized cores.num_cores)
	let part_pairs = indices info.partition_pairs_0
		|> map (\i ->
			(info.partition_pairs_0[i], info.partition_pairs_1[i])
		)
	in dclust2_f64.mk_clusters
		seed_count
		eps
		core_pts
		(cores.core_pids |> sized cores.num_cores)
		part_pairs
		(core_info.part_core_is |> sized info.num_parts)
		(core_info.part_core_sz |> sized info.num_parts)
		(info.part_pairs_is     |> sized info.num_parts)
		(info.part_pairs_sz     |> sized info.num_parts)

entry assign_cluster_ids_2d_f64 [n]
	(seed_count : i64)
	(eps : f64)
	(dat       : indexed_data_2d_f64 [n])
	(info_bd   : partition_info_2d_f64 [n])
	(cores     : isolated_core_pts_2d_f64)
	(core_info : part_core_info_2d_f64)
	(core_cids : []i64)
: [n]i64 =
	let pts = cols_to_vectors
		dat.xs dat.ys
	let core_pts = cols_to_vectors
		(cores.core_xs |> sized cores.num_cores)
		(cores.core_ys |> sized cores.num_cores)
	let part_pairs_bd = indices info_bd.partition_pairs_0
		|> map (\i ->
			(info_bd.partition_pairs_0[i], info_bd.partition_pairs_1[i])
		)
	in dclust2_f64.assign_cluster_ids
		seed_count
		eps
		pts
		info_bd.pids
		core_pts
		(core_cids |> sized cores.num_cores)
		(cores.core_is |> sized cores.num_cores)
		cores.non_core_is
		part_pairs_bd
		(core_info.part_core_is |> sized info_bd.num_parts)
		(core_info.part_core_sz |> sized info_bd.num_parts)
		(info_bd.part_pairs_is  |> sized info_bd.num_parts)
		(info_bd.part_pairs_sz  |> sized info_bd.num_parts)

type dbscan_result_2d_f64 [n] = {
	is_core : [n]bool,
	cluster_id : [n]i64
}

entry deindex_results_2d_f64 [n]
	(dat : indexed_data_2d_f64 [n])
	(is_core : [n]bool)
	(cluster_id : [n]i64)
: dbscan_result_2d_f64 [n] =
	let is_core' = scatter (replicate n false) dat.og_is is_core
	let cluster_id' = scatter (replicate n 0) dat.og_is cluster_id
	in {is_core = is_core', cluster_id = cluster_id'}




