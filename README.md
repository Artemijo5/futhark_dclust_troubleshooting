Troubleshooting futhark implementation for a DBSCAN algorithm based on CUDA-DClust+.
https://github.com/l3lackcurtains/fast-cuda-gpu-dbscan

While attempting to implement this algorithm, it was found that it outputs correct results through sequential C execution, but with the cuda backend it either 'misses' some core points, resulting in incorrect clusters, or runs into weird array access errors.

---

Dataset in 2D_spatial_network.in contains the first 400k elements of the 2D_spatial_network from the above repository, converted into futhark-readable binary format (as [400000][2]f64).

For eps = 0.008, minPts = 8, it should find 398623 core points, 44 clusters (45 including the distinct id assigned to noise points), and a total of 387 noise points (designated by cluster_id == -1).

---

Current status:
- changed some code to use binary searches instead of segmented parallelism. This seemingly "fixes" every `get_partition_info` and `assign_cluster_id`, except the latter seemingly fails for some of the small_test's (but not for 2D_spatial_network...). Original code is preserved in `dclust_alt.fut`, and can be tested by importing that instead of `dclust` in dclust_entry_2d.fut. TODO Might change other routines to use this logic as well?
- the BFS loop causes the test for `mk_clusters` to time out, even though that was not happening in C API execution... TODO see how current code does in C API...

So it's evident that
1. something is going on with segmented scans that ruins `assign_cluster_id` and pids expansion for cuda backend. Probably need to report.
2. either something new appeared with the BFS, or `futhark test` doesn't like it for some reason...

---

How to use:
1. have futhark installed, make sure you can use the c & cuda backends.
2. `futhark pkg sync`
3. The tests in `tests/` directory can be checked with `futhark test --backend=cuda <name_of_file>`, or for sequential execution `futhark test --backend=c <name_of_file>`. The tests in `tests/modular/` can be run by calling `bash ./run.sh cuda` or `bash ./run.sh c` while in that directory.