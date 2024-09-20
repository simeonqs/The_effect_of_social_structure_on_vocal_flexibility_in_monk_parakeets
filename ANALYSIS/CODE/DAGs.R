library(dagitty)
library(rethinking)

set.seed(4)

pdf('ANALYSIS/RESULTS/combined DAG.pdf', 10, 4)
par(mfrow = c(1, 3), oma = c(1, 1, 1, 1))

dag = dagitty('dag{
              MateNetwork -> Similarity;
              MateNetwork -> ForagingNetwork;
              MateNetwork -> SameCluster;
              MateNetwork -> AggressionNetwork;
              MateNetwork -> ToleranceNetwork;
              SameCluster -> Similarity;
              SameCluster -> ForagingNetwork;
              SameCluster -> NestNetwork;
              SameCluster -> AggressionNetwork;
              SameCluster -> ToleranceNetwork;
              NestNetwork -> Similarity;
              NestNetwork -> ForagingNetwork;
              ForagingNetwork -> Similarity;
              ForagingNetwork -> ToleranceNetwork;
              Relatedness -> SameCluster;
              Relatedness -> Similarity
              Relatedness -> MateNetwork;
              AggressionNetwork -> Similarity;
              ToleranceNetwork -> Similarity;
              }') 
drawdag(dag, xlim = c(-4, 4), ylim = c(-4, 4))
text(-3.5, 4, 'a)', font = 2)

dag = dagitty('dag{
              Age -> GroupSize;
              Age -> Diversity;
              Age -> ChamberSize;
              Age -> NetworkPosition;
              Sex -> Diversity;
              Sex -> ChamberSize;
              Sex -> NetworkPosition;
              NetworkPosition -> Diversity;
              ChamberSize -> Diversity;
              ChamberSize -> TreeSize;
              ChamberSize -> NetworkPosition;
              ChamberSize -> GroupSize;
              TreeType -> TreeSize;
              TreeSize -> Diversity;
              TreeSize -> GroupSize;
              TreeSize -> NetworkPosition;
              GroupSize -> NetworkPosition;
              Location -> TreeType;
              Location -> NetworkPosition;
              Location -> GroupSize;
              }') 

drawdag(dag, xlim = c(-4, 4), ylim = c(-4, 4))
text(-3.5, 4, 'b)', font = 2)

dag = dagitty('dag{
              Age -> GroupSize;
              Age -> InformationContent;
              Age -> ChamberSize;
              Age -> Degree;
              Sex -> ChamberSize;
              Sex -> Degree;
              Degree -> InformationContent;
              ChamberSize -> TreeSize;
              ChamberSize -> Degree;
              ChamberSize -> GroupSize;
              TreeType -> TreeSize;
              TreeSize -> InformationContent;
              TreeSize -> GroupSize;
              TreeSize -> Degree;
              GroupSize -> Degree;
              }') 

drawdag(dag, xlim = c(-4, 4), ylim = c(-4.5, 4.5))
text(-3.5, 4.5, 'c)', font = 2)

dev.off()
