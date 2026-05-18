import "../../dclust_entry_2d"

-- Test step #7 - make clusters from core points.
-- ==
-- entry: test7_2d_f64
-- input @data/test6.out
-- output @data/test7.out

entry test7_2d_f64 [n]
	(xs : [n]f64)
	(ys : [n]f64)
	(subdiv : [vector_2.length]i64)
	(part_is : []i64)
	(og_is : [n]i64)
	(num_parts : i64)
	(part_pairs_0 : []i64)
	(part_pairs_1 : []i64)
	(part_sz : []i64)
	(part_pairs_is : []i64)
	(part_pairs_sz : []i64)
	(pids : [n]i64)
	(is_core : [n]bool)
	(num_cores : i64)
	(core_xs : []f64)
	(core_ys : []f64)
	(core_pids : []i64)
	(core_is : []i64)
	(non_core_is : []i64)
	(part_core_sz : []i64)
	(part_core_is : []i64)
=
	let info : partition_info_2d_f64 [n] = {
		num_parts = num_parts,
		partition_pairs_0 = part_pairs_0,
		partition_pairs_1 = part_pairs_1,
		part_sz = part_sz,
		part_pairs_is = part_pairs_is,
		part_pairs_sz = part_pairs_sz,
		pids = pids
	}
	let cores : isolated_core_pts_2d_f64 = {
		num_cores = num_cores,
		core_xs = core_xs,
		core_ys = core_ys,
		core_pids = core_pids,
		core_is = core_is,
		non_core_is = non_core_is
	}
	let core_info : part_core_info_2d_f64 = {
		part_core_sz = part_core_sz,
		part_core_is = part_core_is
	}
	let core_cids = mk_clusters_2d_f64 2048 0.008 info cores core_info
	in (
		xs, ys, subdiv, part_is, og_is,
		is_core,
		num_cores, core_xs, core_ys,
		core_pids, core_is, non_core_is,
		part_core_sz, part_core_is,
		core_cids
	)