library(mixOmics)
library(parallelMap)

Y <- as.factor(binary_class) # Assign a dependent variable to Y.
X <- cbind(CT, PET) # Concatenete different feature blocks and assign them to X. 
list.keepX <- c(seq(1, 8, 1)) # Set up arbitrarily the number of variables keepX.

ncomp = 4 # Set the number of PLS components.
library(parallelMap)
parallelStartMulticore(cpus = 10, mc.set.seed = TRUE) # multicore processing using 10 cores
set.seed(1, "L'Ecuyer")

# Tune DIABLO with 20 repeated 5-fold cross-validation
tune.splsda <- mixOmics::tune.splsda(X1, Y, ncomp = ncomp, validation = 'Mfold', folds = 5, near.zero.var = TRUE, dist="max.dist", 
                                                 test.keepX = list.keepX, nrepeat = 20, cpus=10, auc=TRUE)
parallelStop()

tune.splsda$choice.keepX # Selected number of features
tune.splsda$choice.ncomp$ncomp # Selected number of components

choice.ncomp <- tune.splsda$choice.ncomp$ncomp
choice.keepX <- tune.splsda$choice.keepX

MyResult.splsda <- splsda(X1, Y, ncomp = choice.ncomp, keepX=choice.keepX) # Train sPLS-DA with selected features for each component.
selectedVariables <- selectVar(MyResult.splsda, comp = choice.ncomp) # Selected features for the last selected component.

set.seed(1, "L'Ecuyer")
perf.block.splsda <- perf(MyResult.diablo, validation = "Mfold", folds = 5, progressBar = TRUE, auc = TRUE, nrepeat = 20, dist = "max.dist") 
perf.block.splsda$auc # Cross-validated AUC with 20 repeated 5-fold cross-validation

perf.plsda <- perf(MyResult.splsda, validation = "Mfold", folds = 5, progressBar = TRUE, auc = TRUE, nrepeat = 20, dist = "max.dist") 
perf.plsda$auc

