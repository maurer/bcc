#include <igraph/igraph.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
  igraph_i_set_attribute_table(&igraph_cattribute_table);

  igraph_t graph;
  int err = igraph_read_graph_graphml(&graph, stdin, 0);
  switch (err) {
    case IGRAPH_PARSEERROR:
      fprintf(stderr, "Graph input parse error.\n");
      return 1;
    case IGRAPH_UNIMPLEMENTED:
      fprintf(stderr, "GraphML parsing not enabled in this igraph build.\n");
      return 1;
    case 0:
      break;
    default:
      fprintf(stderr, "Unknown error while loading graph: %d\n", err);
      return 1;
  }

  igraph_strvector_t names;
  igraph_strvector_init(&names, 0);
  VASV(&graph, "func_name", &names);

  igraph_vector_t clustering, modularity;
  igraph_vector_init(&clustering, 0);
  igraph_vector_init(&modularity, 0);

  igraph_matrix_t merges;
  igraph_matrix_init(&merges, 0, 0);

  igraph_to_undirected(&graph, IGRAPH_TO_UNDIRECTED_COLLAPSE, NULL);

  igraph_community_fastgreedy(&graph, NULL, &merges, &modularity, &clustering);

  size_t i;
  for (i = 0; i < igraph_vector_size(&clustering); i++) {
    printf("%li %s\n", (long int)VECTOR(clustering)[i], STR(names, i));
  }
  printf("\n");

  return 0;
}
