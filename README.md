Troubleshooting futhark implementation for a DBSCAN algorithm based on CUDA-DClust+.
https://github.com/l3lackcurtains/fast-cuda-gpu-dbscan

While attempting to implement this algorithm, it was found that it outputs correct results through sequential C execution, but with the cuda backend it either 'misses' some core points, resulting in incorrect clusters, or runs into weird array access errors.

---

Dataset in 2D_spatial_network.in contains the first 400k elements of the 2D_spatial_network from the above repository, converted into futhark-readable binary format (as [400000][2]f64).

For eps = 0.008, minPts = 8, it should find 398623 core points, 44 clusters (45 including the distinct id assigned to noise points), and a total of 387 noise points (designated by cluster_id == -1).

---

How to use:
1. have futhark installed, make sure you can use the c & cuda backends.
2. `futhark pkg sync`
3. The tests in `tests/` directory can be checked with `futhark test --backend=cuda <name_of_file>`, or for sequential execution `futhark test --backend=c <name_of_file>`. The tests in `tests/modular/` can be run by calling `bash ./run.sh cuda` or `bash ./run.sh c` while in that directory.