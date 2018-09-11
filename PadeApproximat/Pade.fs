module Pade
open LUDecomposition
open Utils
  type fractionpoly = { Num: double[]; Den: double[]}
  let pade (a : double[]) : fractionpoly = 
    let n = (a.Length-1) / 2
    let mutable q   = Array2D.zeroCreate n n
    let mutable y = Array.zeroCreate<double> n
    // Set up matrix for solving
    for j = 0 to n-1 do
      y.[j] <- a.[n + j + 1]
      for k = 0 to n-1 do
        q.[j,k] <- a.[j-k+n]
    // Solve by LU decomposition and backsubstitution, with iterative improvement.
    let decomp = LUDecompose q
    let mutable x = LUSolve decomp y
    for j = 0 to 5 do
      x <- LUimprove decomp y x
    // Calculate the remaining coefficients
    for k = 0 to n-1 do
      let mutable sum = a.[k+1]
      for j = 0 to k do
        sum <- sum - x.[j] * a.[k-j]
      y.[k] <- sum
    let numer = Array.init<double> (n+1) (fun j -> 
      if j = 0 then 
        a.[0]
      else
        y.[j-1]
    )
    let denom = Array.init<double> (n+1) (fun j -> 
      if j = 0 then 
        1.0
      else
        -x.[j-1]
    )
    { Num=numer; Den=denom }