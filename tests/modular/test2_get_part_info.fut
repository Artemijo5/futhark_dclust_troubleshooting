import "../../dclust_entry_2d"

-- Test step #2 - Get partition info.
-- ==
-- entry: test2_2d_f64
-- input @data/test1.out
-- output @data/test2.out

entry test2_2d_f64 [n]
	(xs : [n]f64)
	(ys : [n]f64)
	(subdiv : [vector_2.length]i64)
	(part_is : []i64)
	(og_is : [n]i64)
=
	let dat : indexed_data_2d_f64 [n] = {
		xs = xs,
		ys = ys,
		subdiv = subdiv,
		part_is = part_is,
		og_is = og_is
	}
	let info = get_part_info_2d_f64 false dat
	in (
		xs, ys, subdiv, part_is, og_is,
		info.num_parts, info.partition_pairs_0, info.partition_pairs_1,
		info.part_sz, info.part_pairs_is, info.part_pairs_sz, info.pids
	)