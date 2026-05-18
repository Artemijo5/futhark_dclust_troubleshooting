#! /bin/bash

backend=$1

futhark test --backend=$backend test1_index_dataset.fut
futhark test --backend=$backend test2_get_part_info.fut
futhark test --backend=$backend test3_get_neigh_counts.fut
futhark test --backend=$backend test4_get_is_core.fut
futhark test --backend=$backend test5_isolate_core_pts.fut
futhark test --backend=$backend test6_get_part_core_info.fut
futhark test --backend=$backend test7_mk_clusters.fut
futhark test --backend=$backend test8_get_part_info_bd.fut
futhark test --backend=$backend test9_assign_cluster_ids.fut
futhark test --backend=$backend test10_deindex_data.fut