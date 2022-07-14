==> devtools::check(args = c('--as-cran'))

ℹ Updating LKT documentation
ℹ Loading LKT
Loading required package: SparseM

Attaching package: 'SparseM'

The following object is masked from 'package:base':

    backsolve

Loading required package: Matrix
Loading required package: data.table
Loading required package: LiblineaR
Loading required package: HDInterval
Package 'LKT' version 1.2.0
Type 'citation("LKT")' for citing this R package in publications.
── Building ──────────────────────────────────────── LKT ──
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
───────────────────────────────────────────────────────────
✔  checking for file 'C:\Users\ppavl\Dropbox\Active projects\LKT/DESCRIPTION' ...
─  preparing 'LKT': (1.4s)
✔  checking DESCRIPTION meta-information ... 
─  installing the package to build vignettes
✔  creating vignettes (12.8s)
─  checking for LF line-endings in source and make files and shell scripts (696ms)
─  checking for empty or unneeded directories
─  building 'LKT_1.2.0.tar.gz'
   
── Checking ──────────────────────────────────────── LKT ──
Setting env vars:
• _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
• _R_CHECK_CRAN_INCOMING_       : FALSE
• _R_CHECK_FORCE_SUGGESTS_      : FALSE
• NOT_CRAN                      : true
── R CMD check ────────────────────────────────────────────
─  using log directory 'C:/Users/ppavl/Dropbox/Active projects/LKT.Rcheck'
─  using R version 4.2.1 (2022-06-23 ucrt)
─  using platform: x86_64-w64-mingw32 (64-bit)
─  using session charset: UTF-8
─  using options '--no-manual --as-cran'
✔  checking for file 'LKT/DESCRIPTION' ...
─  this is package 'LKT' version '1.2.0'
─  package encoding: UTF-8
✔  checking package namespace information ...
✔  checking package dependencies (4.2s)
✔  checking if this is a source package ...
✔  checking if there is a namespace ...
✔  checking for executable files (716ms)
✔  checking for hidden files and directories ... 
✔  checking for portable file names ... 
✔  checking whether package 'LKT' can be installed (10.7s)
✔  checking installed package size ... 
✔  checking package directory (359ms)
✔  checking for future file timestamps (607ms)
✔  checking 'build' directory ...
✔  checking DESCRIPTION meta-information (359ms)
✔  checking top-level files
✔  checking for left-over files ...
✔  checking index information ... 
✔  checking package subdirectories ... 
✔  checking R files for non-ASCII characters ... 
✔  checking R files for syntax errors ... 
✔  checking whether the package can be loaded (1.5s)
✔  checking whether the package can be loaded with stated dependencies (1.4s)
✔  checking whether the package can be unloaded cleanly (1.5s)
✔  checking whether the namespace can be loaded with stated dependencies (1.2s)
✔  checking whether the namespace can be unloaded cleanly (1.4s)
✔  checking loading without being on the library search path (1.5s)
✔  checking dependencies in R code (1.4s)
✔  checking S3 generic/method consistency (2.4s)
✔  checking replacement functions (1.4s)
✔  checking foreign function calls (1.5s)
✔  checking R code for possible problems (9s)
✔  checking Rd files ... 
✔  checking Rd metadata ... 
✔  checking Rd line widths ... 
✔  checking Rd cross-references ... 
✔  checking for missing documentation entries (1.9s)
✔  checking for code/documentation mismatches (4.8s)
✔  checking Rd \usage sections (2.6s)
✔  checking Rd contents ... 
✔  checking for unstated dependencies in examples ... 
✔  checking contents of 'data' directory (1.7s)
✔  checking data for non-ASCII characters (2s)
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ... 
✔  checking installed files from 'inst/doc' ... 
✔  checking files in 'vignettes' ... 
✔  checking examples (5.3s)
✔  checking for unstated dependencies in vignettes ... 
✔  checking package vignettes in 'inst/doc' ... 
✔  checking re-building of vignette outputs (2.8s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
   
   
── R CMD check results ───────────────────── LKT 1.2.0 ────
Duration: 1m 6.6s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
