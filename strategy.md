# BCC (Binary Constant Clustering)

## What is this?
Binary Constant Clustering is intended to be a method for detecting unusual use of constants in code and directing a manual analyst to take a closer look.

## Execution Strategy

The general approach is to look at many "good" examples that implement the algorithm in question, and identify common components and their constant fingerprints. Then, when evaluating a new piece of software, we try to use the constant fingerprints to match pieces of code to other pieces of code, and look for anomolous constant uses in that.

### Learning Phase

Learning samples are any believed good/correct samples of compiled code which accomplish similar tasks. For example, if you want to vet a webserver, loading up apache, nginx, lighttpd, openssl, and gnutls would be a good idea, because they all accomplish similar tasks.

For each sample, we first convert them into a per-sample constant profile. For every program point which uses a constant, we filter by a heuristic[^heuristic], then cluster by distance between the program points. For each cluster, we output a pair between the set of constants[^constants] and any location information available for the debug information for those program points (e.g. a list of function names involved in the cluster)

Next, we pool all these cluster profiles together, and cluster by the Jaccard index between constant pools for each of the previous code clusters. For each of these new clusters, we take the multiset union of the previous constant pools (multiset here so that constants which occur in many exemplars can be weighted more heavily later), and the union of the debug information affiliated with the cluster. Optionally, the debug information can be checked for representative information, such as an exceptionally common substring of function names, for later summary display to the user.

### Testing Phase

First, take the binary to be tested, and perform the code region clustering the same way that it was done on the training data, but keep the program points affiliated with the constant pools this time, as we will need them later.

Next, attempt to assign code clusters to constant clusters in the trained information via weighted Jaccard - that is, if the cluster has n instances of constant k, and constant k is used at least once in the test data, it counts n times towards the intersection.

For each code cluster which has a constant not found in the cluster to which it was assigned via weighted Jaccard, add it to a list of potential problems. Sort this list by how well the weighted Jaccard says it matches the cluster, with higher being higher priority.

Display each program point/constant pair to the user, prioritized, along with debug information from the clustered training data.

[^heuristic]: To avoid skewing the constant pools and generating excessive load on the clustering system, some extraordinarily common constants (0, 1, 2, 4, 8, others as needed) may be ignored in favor of more interesting constants.
[^constants]: To ignore address space artifacts, we project the constants through a normalization procedure. If the constant is a pointer into .rodata, we use either the 8 byte value at that location, or if it is a valid UTF-8 or ASCII string, the string at that location. Otherwise, we use the constant at the size it appears in the code.

## Decisions

* What distance metric should I use between points of code?
* How do I assign code clusters to a trained cluster? Max Jaccard?
* How do I say that a code cluster does not match a trained cluster? Minimum required Jaccard? How do I pick a threshold?
* Code clustering will need some balance between cluster count and density of things in a cluster. How do I set that? Hierarchical clustering? Pick a constant?
