#### kDetect - a kernel-layer strace
=====================================

##### 1. Introduction

A powerful yet simple strace based on SystemTap

##### 2. Prerequisites

  * SystemTap (http://sourceware.org/systemtap/)
  * Glasgow Haskell Compiler (http://www.haskell.org/ghc/)
  
##### 3. Compiler

  1. ghc --make Main.hs Detectors.hs Trace.hs -o cate
  2. chmod a+x observation.stp
  3. chmod a+x obs_filter
  
##### 4. Tester

  * run with process id: 
    sudo ./observation.stp -x process_id | ./obs_filter | ./cate
    
  * run with program name:
    sudo ./observation.stp -c "path_to_program args" | ./obs_filter | ./cate
  
