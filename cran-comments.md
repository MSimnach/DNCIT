## R CMD check results

0 errors | 0 warnings | 3 note

* This is a new release.

* The following NOTE is challenging to resolve: 
  checking CRAN incoming feasibility ... [14s] NOTE (13.8s)
   Maintainer: 'Marco Simnacher <marco.simnacher@hu-berlin.de>'
   
   New submission
   
   Version contains large components (0.0.0.9000)
   
   Unknown, possibly misspelled, fields in DESCRIPTION:
     'Remotes'
   
   Suggests or Enhances not in mainstream repositories:
     RCIT
  
The RCIT package is only available on Github. It is not necessary for the functionality of the package, but it increases the performance of the package significantly. So I added it to the Suggests field in the DESCRIPTION file with the Remotes comment. I hope this is acceptable.

* checking examples ... [361s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
           user system elapsed
  cmiknn   7.23   2.11    8.09
  pred_cit 6.00   0.51    6.28
  r_fcit   0.72   0.52    9.68
  
The examples can take more than 5s, depending on the system. Unfortunately, this is the case even for the lowest dimension and sample size in the examples, since the functions used take more than 5s even in the simplest setting. I hope this is acceptable.
     
