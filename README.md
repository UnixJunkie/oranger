# oranger
thin OCaml wrapper for the ranger (C++) random forests implementation

ranger is here:
https://github.com/imbs-hl/ranger

Ranger's command line help:
```
# ranger --help
Usage: 
    ranger [options]

Options:
    --help                        Print this help.
    --version                     Print version and citation information.
    --verbose                     Turn on verbose mode.
    --file FILE                   Filename of input data. Only numerical values are supported.
    --treetype TYPE               Set tree type to:
                                  TYPE = 1: Classification.
                                  TYPE = 3: Regression.
                                  TYPE = 5: Survival.
                                  (Default: 1)
    --probability                 Grow a Classification forest with probability estimation for the classes.
                                  Use in combination with --treetype 1.
    --depvarname NAME             Name of dependent variable. For survival trees this is the time variable.
    --statusvarname NAME          Name of status variable, only applicable for survival trees.
                                  Coding is 1 for event and 0 for censored.
    --ntree N                     Set number of trees to N.
                                  (Default: 500)
    --mtry N                      Number of variables to possibly split at in each node.
                                  (Default: sqrt(p) with p = number of independent variables)
    --targetpartitionsize N       Set minimal node size to N.
                                  For Classification and Regression growing is stopped if a node reaches a size smaller than N.
                                  For Survival growing is stopped if one child would reach a size smaller than N.
                                  This means nodes with size smaller N can occur for Classification and Regression.
                                  (Default: 1 for Classification, 5 for Regression, and 3 for Survival)
    --maxdepth N                  Set maximal tree depth to N.
                                  Set to 0 for unlimited depth. A value of 1 corresponds to tree stumps (1 split).
    --catvars V1,V2,..            Comma separated list of names of (unordered) categorical variables. 
                                  Categorical variables must contain only positive integer values.
    --write                       Save forest to file <outprefix>.forest.
    --predict FILE                Load forest from FILE and predict with new data. The new data is expected in the exact same 
                                  shape as the training data. If the outcome of your new dataset is unknown, add a dummy column.
    --predall                     Return a matrix with individual predictions for each tree instead of aggregated 
                                  predictions for all trees (classification and regression only).
    --predictiontype TYPE         Set type of prediction to:
                                  TYPE = 1: Return predicted classes or values.
                                  TYPE = 2: Return terminal node IDs per tree for new observations.
                                  (Default: 1)
    --impmeasure TYPE             Set importance mode to:
                                  TYPE = 0: none.
                                  TYPE = 1: Node impurity: Gini for Classification, variance for Regression, sum of test statistic for Survival.
                                  TYPE = 2: Permutation importance, scaled by standard errors.
                                  TYPE = 3: Permutation importance, no scaling.
                                  TYPE = 5: Corrected node impurity: Bias-corrected version of node impurity importance.
                                  (Default: 0)
    --noreplace                   Sample without replacement.
    --fraction X                  Fraction of observations to sample. Default is 1 for sampling with replacement 
                                  and 0.632 for sampling without replacement.
    --splitrule RULE              Splitting rule:
                                  RULE = 1: Gini for Classification, variance for Regression, logrank for Survival.
                                  RULE = 2: AUC for Survival, not available for Classification and Regression.
                                  RULE = 3: AUC (ignore ties) for Survival, not available for Classification and Regression.
                                  RULE = 4: MAXSTAT for Survival and Regression, not available for Classification.
                                  RULE = 5: ExtraTrees for all tree types.
                                  (Default: 1)
    --randomsplits N              Number of random splits to consider for each splitting variable (ExtraTrees splitrule only).
    --alpha VAL                   Significance threshold to allow splitting (MAXSTAT splitrule only).
    --minprop VAL                 Lower quantile of covariate distribtuion to be considered for splitting (MAXSTAT splitrule only).
    --caseweights FILE            Filename of case weights file.
    --holdout                     Hold-out mode. Hold-out all samples with case weight 0 and use these for variable 
                                  importance and prediction error.
    --splitweights FILE           Filename of split select weights file.
    --alwayssplitvars V1,V2,..    Comma separated list of variable names to be always considered for splitting.
    --skipoob                     Skip computation of OOB error.
    --nthreads N                  Set number of parallel threads to N.
                                  (Default: Number of CPUs available)
    --seed SEED                   Set random seed to SEED.
                                  (Default: No seed)
    --outprefix PREFIX            Prefix for output files.
    --memmode MODE                Set memory mode to:
                                  MODE = 0: double.
                                  MODE = 1: float.
                                  MODE = 2: char.
                                  (Default: 0)
    --savemem                     Use memory saving (but slower) splitting mode.

See README file for details and examples.
```
