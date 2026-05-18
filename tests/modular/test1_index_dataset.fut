import "../../dclust_entry_2d"

-- Test step #1 - Index Dataset.
-- ==
-- entry: test1_2d_f64
-- input @../data/2D_spatial_network.in
-- output @data/test1.out

entry test1_2d_f64 [n]
	(pts : [n][2]f64)
=
	let pts_t = pts |> transpose
	let xs = pts_t[0]
	let ys = pts_t[1]
	let dat    = index_dataset_2d_f64 0.008 100 xs ys
	in (dat.xs, dat.ys, dat.subdiv, dat.part_is, dat.og_is)