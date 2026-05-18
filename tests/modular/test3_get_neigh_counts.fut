import "../../dclust_entry_2d"

-- Test step #3 - get neighbour counts of each point.
-- ==
-- entry: test3_2d_f64
-- input @data/test2.out
-- output @data/test3.out

entry test3_2d_f64 [n]
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
=
	let dat : indexed_data_2d_f64 [n] = {
		xs = xs,
		ys = ys,
		subdiv = subdiv,
		part_is = part_is,
		og_is = og_is
	}
	let info : partition_info_2d_f64 [n] = {
		num_parts = num_parts,
		partition_pairs_0 = part_pairs_0,
		partition_pairs_1 = part_pairs_1,
		part_sz = part_sz,
		part_pairs_is = part_pairs_is,
		part_pairs_sz = part_pairs_sz,
		pids = pids
	}
	let neigh_counts = get_neighbour_counts_2d_f64 2048 0.008 dat info
	in (
		xs, ys, subdiv, part_is, og_is,
		info.num_parts, info.partition_pairs_0, info.partition_pairs_1,
		info.part_sz, info.part_pairs_is, info.part_pairs_sz, info.pids,
		neigh_counts
	)