import "../../dclust_entry_2d"

-- Test step #4 - get core point flags.
-- ==
-- entry: test4_2d_f64
-- input @data/test3.out
-- output @data/test4.out

entry test4_2d_f64 [n]
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
	(neigh_counts : [n]i64)
=
	let is_core = get_is_core_2d_f64 8 neigh_counts
	in (
		xs, ys, subdiv, part_is, og_is,
		num_parts, part_pairs_0, part_pairs_1,
		part_sz, part_pairs_is, part_pairs_sz, pids,
		is_core
	)