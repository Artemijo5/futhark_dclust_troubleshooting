import "../../dclust_entry_2d"

-- Test step #8 - make bidirectional partition info.
-- ==
-- entry: test8_2d_f64
-- input @data/test7.out
-- output @data/test8.out

entry test8_2d_f64 [n]
	(xs : [n]f64)
	(ys : [n]f64)
	(subdiv : [vector_2.length]i64)
	(part_is : []i64)
	(og_is : [n]i64)
	(is_core : [n]bool)
	(num_cores : i64)
	(core_xs : []f64)
	(core_ys : []f64)
	(core_pids : []i64)
	(core_is : []i64)
	(non_core_is : []i64)
	(part_core_sz : []i64)
	(part_core_is : []i64)
	(core_cids : []i64)
=
	let dat : indexed_data_2d_f64 [n] = {
		xs = xs,
		ys = ys,
		subdiv = subdiv,
		part_is = part_is,
		og_is = og_is
	}
	let info_bd = get_part_info_2d_f64 true dat
	in (
		xs, ys, subdiv, part_is, og_is,
		is_core,
		num_cores, core_xs, core_ys,
		core_pids, core_is, non_core_is,
		part_core_sz, part_core_is,
		core_cids,
		info_bd.num_parts, info_bd.partition_pairs_0, info_bd.partition_pairs_1,
		info_bd.part_sz, info_bd.part_pairs_is, info_bd.part_pairs_sz, info_bd.pids,
	)