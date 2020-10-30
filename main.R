library(mixOmics)
library(parallelMap)

Y <- as.factor(binary_class) # outcome bionary class
X <- list(CT = CT, PET = PET) # CT and PET radiomics feature blocks
list.keepX <- list(CT = c(seq(1, 4, 1)), PET = c(seq(1, 4, 1))) # 

s = 3 # size of design matrix
w = 1 # correlation between data matrices
design = matrix(1, ncol = s, nrow = s)
diag(design) =  0
design[1,s] = w
design[s,1] = w

parallelStartMulticore(cpus=10, mc.set.seed=TRUE)
set.seed(1, "L'Ecuyer")
tune.block.splsda <- mixOmics::tune.block.splsda(X, Y, ncomp = 4, validation = 'Mfold', folds = 5, design=design, near.zero.var = TRUE, light.output = FALSE, dist="max.dist",
                                                 test.keepX = list.keepX, nrepeat = 20, cpus=10)
parallelStop()

tune.block.splsda$choice.keepX
tune.block.splsda$choice.ncomp$ncomp

choice.ncomp <- tune.block.splsda$choice.ncomp$ncomp
choice.keepX <- list(CT=tune.block.splsda$choice.keepX$CT[1:choice.ncomp], PET=tune.block.splsda$choice.keepX$PET[1:choice.ncomp])

MyResult.diablo <- block.splsda(X, Y, ncomp = choice.ncomp, keepX=choice.keepX)
selectedVariables <- selectVar(MyResult.diablo, comp = choice.ncomp)

set.seed(1, "L'Ecuyer")
perf.block.splsda <- perf(MyResult.diablo, validation = "Mfold", folds = 5, progressBar = TRUE, auc = TRUE, nrepeat = 20, dist="max.dist") 
perf.block.splsda$auc
