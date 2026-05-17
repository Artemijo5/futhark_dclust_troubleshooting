import "../dclust_entry_2d"

-- Test total dclust

def test_pts : [][2]f64 = [
	[6,0],
	[0,15],
	[14.4,2],[13.6,2],[14,1.6],[14,2],[14.3,2.6],[13.7,2.6],[14,2.87],[14,2.4],
	[10.1,9.8],[15,8],
	[4.8,4.9],[4.9,4.8],
	[5.1,5.1],[5.55,5.1],[5.1,5.55],[5.8,5.2],[9.6,9.9],
	[1,9.9],[0.6,9.9],[1.2,9.7],[0.8,9.7],
	[1,10.35],
	[9.8,10.5],[9.8,10.1],
	[10.2,10.2]
]

def do_dclust_2d_f64 [n]
	(seed_count : i64)
	(subdiv : i64)
	(eps : f64)
	(minPts : i64)
	(pts : [n][2]f64)
: ([n]bool, [n]i64) =
	let pts_t = pts |> transpose
	let xs = pts_t[0]
	let ys = pts_t[1]
	let dat          = index_dataset_2d_f64 eps subdiv xs ys
	let info         = get_part_info_2d_f64 false dat
	let neigh_counts = get_neighbour_counts_2d_f64 seed_count eps dat info
	let is_core      = get_is_core_2d_f64 minPts neigh_counts
	let cores        = isolate_core_pts_2d_f64 is_core dat info
	let core_info    = get_part_core_info_2d_f64 cores dat
	let core_cids    = mk_clusters_2d_f64 seed_count eps info cores core_info
	let info_bd      = get_part_info_2d_f64 true dat
	let cluster_id   = assign_cluster_ids_2d_f64 seed_count eps dat info_bd cores core_info core_cids
	let res          = deindex_results_2d_f64 dat is_core cluster_id
	in (res.is_core, res.cluster_id)

-- Test on small data.
-- ==
-- entry: small_test
-- input {1i64 1i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64] }
-- input {5i64 1i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64] }
-- input {26i64 1i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false]  [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64] }
-- input {100i64 1i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false]  [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64] }
-- input {1i64 3i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 2i64, 2i64, 2i64, 2i64, 2i64, -1i64, 3i64, 1i64, 1i64, 1i64, 1i64, 1i64, 3i64, 3i64, 3i64] }
-- input {3i64 3i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 2i64, 2i64, 2i64, 2i64, 2i64, -1i64, 3i64, 1i64, 1i64, 1i64, 1i64, 1i64, 3i64, 3i64, 3i64] }
-- input {7i64 3i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 2i64, 2i64, 2i64, 2i64, 2i64, -1i64, 3i64, 1i64, 1i64, 1i64, 1i64, 1i64, 3i64, 3i64, 3i64] }
-- input {26i64 3i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 2i64, 2i64, 2i64, 2i64, 2i64, -1i64, 3i64, 1i64, 1i64, 1i64, 1i64, 1i64, 3i64, 3i64, 3i64] }
-- input {100i64 3i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 2i64, 2i64, 2i64, 2i64, 2i64, -1i64, 3i64, 1i64, 1i64, 1i64, 1i64, 1i64, 3i64, 3i64, 3i64] }
-- input {1i64 15i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64]  }
-- input {3i64 15i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64]  }
-- input {7i64 15i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64]  }
-- input {26i64 15i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64]  }
-- input {100i64 15i64}
-- output { [false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, false, true, false, false, false, false, false, true, false] [-1i64, -1i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 3i64, -1i64, 1i64, 1i64, 1i64, 1i64, 1i64, -1i64, 3i64, 2i64, 2i64, 2i64, 2i64, 2i64, 3i64, 3i64, 3i64]  }

entry small_test (seed_count : i64) (subdiv : i64)
= do_dclust_2d_f64 seed_count subdiv 0.5 5 (copy test_pts)

-- Test on 2D_Spatial_Network dataset, with 100x100 subdivisions.
-- ==
-- entry: test_subdiv_100
-- input @data/2D_spatial_network.in
-- output @data/100_subdiv.out

entry test_subdiv_100 [n] (pts : [n][2]f64)
= do_dclust_2d_f64 2048 100 0.008 8 pts

-- Test on 2D_Spatial_Network dataset, with 200x200 subdivisions.
-- ==
-- entry: test_subdiv_200
-- input @data/2D_spatial_network.in
-- output @data/200_subdiv.out

entry test_subdiv_200 [n] (pts : [n][2]f64)
= do_dclust_2d_f64 2048 200 0.008 8 pts

-- Test on 2D_Spatial_Network dataset, with 1000x1000 subdivisions.
-- ==
-- entry: test_subdiv_1000
-- input @data/2D_spatial_network.in
-- output @data/1000_subdiv.out

entry test_subdiv_1000 [n] (pts : [n][2]f64)
= do_dclust_2d_f64 2048 1000 0.008 8 pts