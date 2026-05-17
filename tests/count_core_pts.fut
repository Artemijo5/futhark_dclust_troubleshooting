import "../dclust_entry_2d"

-- Random points
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

def count_core_pts [n]
	(seed_count : i64)
	(subdiv : i64)
	(eps : f64)
	(minPts : i64)
	(pts : [n][2]f64)
: i64 =
	let pts_t = pts |> transpose
	let xs = pts_t[0]
	let ys = pts_t[1]
	let dat  = index_dataset_2d_f64 eps subdiv xs ys
	let info = get_part_info_2d_f64 false dat
	let neigh_counts = get_neighbour_counts_2d_f64 seed_count eps dat info
	in neigh_counts |> map (\nc -> nc >= minPts)
		|> map (i64.bool)
		|> reduce (+) 0

-- Test correctness on a small dataset.
--
-- For different seed_count & subdiv,
-- minPts from 1 to 6,
-- eps = 0.5
--
-- ==
-- entry: small_test
-- input {1i64 1i64 1i64}
-- output {27i64}
-- input {3i64 3i64 1i64}
-- output {27i64}
-- input {5i64 15i64 1i64}
-- output {27i64}
-- input {1i64 1i64 2i64}
-- output {24i64}
-- input {3i64 3i64 2i64}
-- output {24i64}
-- input {5i64 15i64 2i64}
-- output {24i64}
-- input {1i64 1i64 3i64}
-- output {17i64}
-- input {3i64 3i64 3i64}
-- output {17i64}
-- input {5i64 15i64 3i64}
-- output {17i64}
-- input {1i64 1i64 4i64}
-- output {8i64}
-- input {3i64 3i64 4i64}
-- output {8i64}
-- input {5i64 15i64 4i64}
-- output {8i64}
-- input {1i64 1i64 5i64}
-- output {5i64}
-- input {3i64 3i64 5i64}
-- output {5i64}
-- input {5i64 15i64 5i64}
-- output {5i64}
-- input {1i64 1i64 6i64}
-- output {0i64}
-- input {3i64 3i64 6i64}
-- output {0i64}
-- input {5i64 15i64 6i64}
-- output {0i64}

entry small_test
	(seed_count : i64)
	(subdiv : i64)
	(minPts : i64)
= count_core_pts seed_count subdiv 0.5 minPts (copy test_pts)

-- Test on 2D_spatial_network.in (minPts==1)
-- ==
-- entry: test_minPts_1
-- input @data/2D_spatial_network.in
-- output {400000i64}

entry test_minPts_1 (pts : [][2]f64)
= count_core_pts 2048 100 0.008 1 pts

-- Test on 2D_spatial_network.in (minPts==8)
-- ==
-- entry: test_minPts_8
-- input @data/2D_spatial_network.in
-- output {398623i64}

entry test_minPts_8 (pts : [][2]f64)
= count_core_pts 2048 100 0.008 8 pts