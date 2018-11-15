# <img src="https://github.com/SebastianSosa/ANTs/blob/master/inst/ressources/ANT.png" alt="alt text" width="10%" height="10%">  Animal Network Toolkit Software (ANTs)


Animal Network Toolkit Software (ANTs) is a free open R package for analysing animal networks.

ANTs is a multi-collaborative project and is meant to continuously evolve. If you wish to contribute by providing suggestions or additional codes please contact us at: ant@s-sosa.com

If you need help or if you want to report bugs, please contact us at: ant.help@s-sosa.com

For more information, visit our websites:

   www.s-sosa.com/ants
   
   https://www.researchgate.net/project/A-multilevel-statistical-toolkit-to-study-animal-social-networks-Animal-Network-Toolkit-ANT-R-package
   
   
<i><b>Authors</b>: [Sebastian Sosa](https://scholar.google.fr/citations?user=R8MskkwAAAAJ&hl=fr&authuser=1), [Ivan Puga-Gonzalez](https://scholar.google.fr/citations?user=XIjvceIAAAAJ&hl=fr&oi=ao), Hu Feng He, Xiaohua Xie, [Cédric Sueur](https://scholar.google.fr/citations?user=2A3IqEsAAAAJ&hl=fr&authuser=1)</i>

# Description

How animals interact and develop social relationships in face of sociodemographic and ecological pressures is of great interest. New methodologies, in particular Social Network Analysis (SNA), allow us to elucidate these types of questions. However, the different methodologies developed to that end and the speed at which they emerge make their use difficult. Moreover, the lack of communication between the different software developed to provide an answer to the same/different research questions is a source of confusion. 

The R package ‘Animal Network Toolkit’ (ANTs) was developed with the aim of implementing in one package the different social network analysis techniques currently used in the study of animal social networks. Hence, ANTs is a toolkit for animal research allowing among other things to: 1) measure global, dyadic and nodal networks metrics; 2) perform data randomization: pre- and post-network (node and link permutations); 3) perform statistical permutation tests as correlation test, t-test, General Linear Model, General Linear Mixed Model, deletion simulation, Matrix TauKr correlations. 

The package is partially coded in C++ using the R package Rcpp for an optimal coding speed. 

The package gives researchers a workflow from the raw data to the achievement of statistical analyses, allowing for a multilevel approach: from the individual’s position and role within the network, to the identification of interactional patterns, and the study of the overall network properties. 

Furthermore, ANTs also provides a guideline on the SNA techniques used: 1) from the appropriate randomization technique according to the data collected; 2) to the choice, the meaning, the limitations and advantages of the network metrics to apply, 3) and the type of statistical tests to run. The ANTs project is multi-collaborative, aiming to provide access to advanced social network analysis techniques and to create new ones that meet researchers’ needs in future versions.

# Installation
To install the package in R, use the following command: <i>devtools::install_github("SebastianSosa/ANTs")</i>

Or download and install the package following the standard R installation process.

# Vignette
<img src="https://github.com/SebastianSosa/ANTs/blob/master/vignettes/Analytical.protocols.png" alt="alt text"> 
# Tutorials
To find information on how to use ANTs, follow this link: 
https://www.s-sosa.com/videos

