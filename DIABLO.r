library(mixOmics)
library(parallelMap)

Y <- as.factor(binary_class) # Assign a dependent variable to Y.
X <- list(CT = CT, PET = PET) # Assign feature blocks to X. 
list.keepX <- list(CT = c(seq(1, 4, 1)), PET = c(seq(1, 4, 1))) # Set up arbitrarily the number of variables keepX for each block.

s = 3 # Set size of design matrix (number of feature blocks + 1)
w = 1 # Set connection between different datasets (between 0 and 1)
design = matrix(1, ncol = s, nrow = s)
diag(design) =  0
for(i in 1:s) for(j in 1:s) if(i!=j & i!=s & j!=s) design[i,j]=w

ncomp = 4 # Set the number of PLS components.
library(parallelMap)
parallelStartMulticore(cpus = 10, mc.set.seed = TRUE) # multicore processing using 10 cores
set.seed(1, "L'Ecuyer")

# Tune DIABLO with 20 repeated 5-fold cross-validation
tune.block.splsda <- mixOmics::tune.block.splsda(X, Y, ncomp = ncomp, validation = 'Mfold', folds = 5, design = design, near.zero.var = TRUE, light.output = FALSE, dist = "max.dist",
                                                 test.keepX = list.keepX, nrepeat = 20, cpus=10)
parallelStop()

tune.block.splsda$choice.keepX # Selected number of features for each block
tune.block.splsda$choice.ncomp$ncomp # Selected number of components

choice.ncomp <- tune.block.splsda$choice.ncomp$ncomp
choice.keepX <- list(CT = tune.block.splsda$choice.keepX$CT[1:choice.ncomp], PET = tune.block.splsda$choice.keepX$PET[1:choice.ncomp])

MyResult.diablo <- block.splsda(X, Y, ncomp = choice.ncomp, keepX = choice.keepX) # Train DIABLO with selected features for each block for each component.
selectedVariables <- selectVar(MyResult.diablo, comp = choice.ncomp) # Selected features for each block for the last selected component.

set.seed(1, "L'Ecuyer")
perf.block.splsda <- perf(MyResult.diablo, validation = "Mfold", folds = 5, progressBar = TRUE, auc = TRUE, nrepeat = 20, dist = "max.dist") 
perf.block.splsda$auc # Cross-validated AUC with 20 repeated 5-fold cross-validation

