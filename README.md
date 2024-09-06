# Generalized Likelihood Uncertainty Estimation Parallelized 
---

The GLUE (Generalized Likelihood Uncertainty Estimation) method is used to estimate genotype-specific coefficients 
for the DSSAT crop models. It is a Bayesian estimation approach that uses Monte Carlo sampling from prior distributions 
of the coefficients and a Gaussian likelihood function to determine the best coefficients based on the data used in the estimation process. More information can be found on the [User Guideline documentation][link1].


The Generalized Likelihood Uncertainty Estimation Parallelized (GLUEP) leverages the GLUE method and parallel processing to significantly reduce the time required for estimating the genotype-specific coefficients (GSPs). The software divides into chunks the total number of simulations according to the number of cores (CPUs) available.
Each CPU core processes the simulations independently, and the results are combined afterward to evaluate which
parameter set of GSPS was able to represent better the data observations provided.

**The GLUEP uses the default R library "parallel" for parallel processing through network sockets.**


---
## _How to use it_

- Create a "SimulationControl.csv" file. This is a configuration file for running GLUE. Find below the required structure 
  for the file.

- Create a C file (similar to a batch file) and put it in your working directory. 
  RUNNING THIS GLUE VERSION IN THE COMMAND LINE WILL NOT CREATE THE C FILE (Use the GLUESelect interface to generate the file).

- Check if the cultivar to be calibrated is defined inside the .CUL with a unique ID and some initial values
  for the genotype-specific coefficients. GLUE will use the "MINIMA" and "MAXIMA" inside the cultivar/ecotype file to 
  estimate the new genetic coefficients. Also, make sure that your genotype files have a "!Calibration" line 
  identifying which coefficients are related to phenology ("P"), growth ("G"), and not applicable ("N").
  
- On the command prompt/terminal, go to the GLUE directory and run GLUE using the command: 
    ```sh
        Rscript GLUE.r
    ```
- Windows users should specify the full path to Rscript (Usually located in C:\Program Files\R\...\bin\Rscript.exe).

- GLUEP will calibrate the GSPs inside the ecotype file if ecotypeCalibration equals "Y".
  
- To save previous runs, please move the results to another folder or create a folder named "BackUp" inside your output 
  directory (usually defined as GLWork).

---
### _Structure of simulation control file_

 - CultivarBatchFile - Define the file C (batch) to be used (the file should be located inside the GLWork/working directory).
 - ModelID - Inform which model should be used for the calibration (Tip: All model IDs can be found in the DSCSM048.CTR).
 - EcotypeCalibration - Indicates if GLUE should also calibrate the coefficients on the respective ecotype (.ECO) file. 
  --                  EcotypeCalibration = "Y" indicates that GLUE should also calibrate the Ecotype coefficients for this 
                    cultivar. The .ECO file MUST be well structured with "MAXIMA" and "MINIMA" and each coefficient and
                    MUST contain a header indicating whether it is associated with phenology ("P"), dry mass growth ("G"), or 
                    not applicable ("N").
 - GLUED - Define the path for the GLUE directory.
 - OutputD - Define the directory path for the working directory and cultivar calibration outputs.
 - DSSATD - Define the directory path where DSSAT is located.
 - GenotypeD - Define the directory path where the genotype-related files are located (.CUL, .ECO, .SPE).
 - GLUEFlag - Define the flag for the GLUE procedure. 
-- GLUEFlag = 1, both coefficients related to phenology and growth will be evaluated; 
-- GLUEFlag = 2, only phenology will be evaluated; 
-- GLUEFlag = 3, only growth will be evaluated;
 - NumberOfModelRun - Define the number of model runs for each treatment.
 - Cores - Indicate the number of cores (CPUs) to run the coefficient calibration. If Cores = "", GLUE will assume 
           that it is running on a High Performance Computer (SLURM job scheduler) and use the same amount of cores
           specified through the "--cpus-per-task" command (usually defined in the SLURM job request file - .sh).
           *Use "parallel::detectCores()" command in R or RStudio to check the number of cores available. When 
           requesting more cores than what is actually available, GLUE will use the maximum number of cores - 1 to
           execute the calibration.*
           **We do not recommend the use of all available cores in your machine for running GLUE.**

---
  **NOTES**
  - The size of the soil profile file (e.g. .SOL) impacts the time required to run the cultivar calibration.
    It is recommended to create a soil file with only the profiles being used during the calibration;
  **Windows users running this application from the command prompt** need to use "/" instead of "\" when defining the paths in the SimulationControl.csv file.

---

### _General information_

GLUEP is currenly maintained by [Thiago Berton Ferreira][contactlink1], [Vakthang Shelia][contactlink2] and [Gerrit Hoogenboom][contactlink3].

Contributions are always welcome via "Issues" in the [issue-board][link2].

[link1]: <https://usermanual.wiki/Document/Glue20Users20Guide20Version2047.1359076516.pdf>
[link2]: <https://github.com/DSSAT/glue/issues>
[contactlink1]: <https://github.com/thiagoferreira53>
[contactlink2]: <https://github.com/vshelia>
[contactlink3]: <https://github.com/GerritHoogenboom>


