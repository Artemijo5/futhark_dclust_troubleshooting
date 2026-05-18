import "../../dclust_entry_2d"

-- Test step #10 - de-index results.
-- ==
-- entry: test10_2d_f64
-- input @data/test9.out
-- output @../data/100_subdiv.out

entry test10_2d_f64 [n]
	(xs : [n]f64)
	(ys : [n]f64)
	(subdiv : [vector_2.length]i64)
	(part_is : []i64)
	(og_is : [n]i64)
	(is_core : [n]bool)
	(cluster_id : [n]i64)
=
	let dat : indexed_data_2d_f64 [n] = {
		xs = xs,
		ys = ys,
		subdiv = subdiv,
		part_is = part_is,
		og_is = og_is
	}
	let res = deindex_results_2d_f64 dat is_core cluster_id
	in (
		res.is_core,
		res.cluster_id
	)