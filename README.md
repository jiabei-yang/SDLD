# Subgroup Discovery for Longitudinal Data (SDLD) Algorithm

Code for paper [Yang, J., Mwangi, A. W., Kantor, R., Dahabreh, I. J., Nyambura, M., Delong, A., ... & Steingrimsson, J. A. (2022). Tree-based Subgroup Discovery Using Electronic Health Record Data: Heterogeneity of Treatment Effects for DTG-containing Therapies.](https://arxiv.org/abs/2208.14329)

* `main/`: code for data analysis in the main manuscript.
  * Analysis of the AMPATH data: Because of confidentiality issues, we do not share the code used for analyzing the dataset. `Appendix/` contains code for fitting SDLD algorithm on simulated data with a similar design to the AMPATH data.
    + `seed1000.rda`: seeds for used in code.
    
* `Functions/`: functions to implement the SDLD algorithm.
  + `CvMethod1.R`: functions for the final tree selection method described in the main manuscript. 
  + `deterministicFunction.R`: sample deterministic g function in `ltmle()`. 
  + `EstLtmleTempFunc.R`: splitting and evaluation functions with targeted maximum likelihood estimator (TMLE). 
  + `EvalMeas.R`: functions to evaluate final trees with the evaluation criteria described for simulations.
  + `gen.R`: suppress messages and warnings from program. 
  + `iTemp.R`: initialization function.
  + `library.R`: install required libraries, if not yet installed, and load the libraries.
  + `pruning.R`: build a maximum sized tree and create a sequence of candidate trees for final tree selection.
  + `SimData.R`: simulate data under data generating distribution in Appendix C.1.
  + `SimRealData.R`: simulate data with a similar design to the dataset in main manuscript.

* `Appendix/`: Additional code for results in Web Appendices.  
  + `Appendix*.R`: code for Appendix *.
  + `AppendixC3*.R`: ode for fitting SDLD algorithm on simulated data with a similar design to the AMPATH data.

* `Data/`: simulated data with a similar design to the dataset in main manuscript.
  