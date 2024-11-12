## R CMD check results

0 errors | 0 warnings | 2 note

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
  
The RCIT package is only available on Github. It is not necessary for the functionality of the package, but it increases the     performance of the package significantly. Thus, I have added it to the Suggests field in the DESCRIPTION file with the Remotes remark. I hope this is acceptable.


     
