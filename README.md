# misclassification_dengue
Adjusting for misclassification of muscle pain in pooled data on the diagnosis of dengue

This is the code for the motivating example on the diagnosis of dengue using the potentially misclassified muscle pain variable, and a joint pain variable, measured in multiple pooled data sets.

The structure of the code is as follows: sampling_utils.R and sampling_misclass_re.R are used for sampling binary clustered data with potentially misclassified exposure. The code for the example on diagnosing dengue using the potentially misclassified muscle pain variable, and the joint pain variable is in run_denv_ofi.R, which uses the R and JAGS functions in the equations folder, and uses the functions in utils.R to extract the necessary info.
