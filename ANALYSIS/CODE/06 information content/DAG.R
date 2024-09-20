library(dagitty)
library(rethinking)
pdf('ANALYSIS/RESULTS/07 information content/DAG information.pdf')
dag = dagitty('dag{
              Age -> GroupSize;
              Age -> Information;
              Age -> EntrySize;
              Age -> Degree;
              Sex -> EntrySize;
              Sex -> Degree;
              Degree -> Information;
              EntrySize -> TreeSize;
              EntrySize -> Degree;
              EntrySize -> GroupSize;
              TreeType -> TreeSize;
              TreeSize -> Information;
              TreeSize -> GroupSize;
              TreeSize -> Degree;
              GroupSize -> Degree;
              }') 
drawdag(dag)
dev.off()
