module LUDecomposition
open Utils

  type PALU = {P : double[,]; A : double[,]; L : double[,]; U : double[,]}
  let eye (n : int) : double[,] = 
    Array2D.init<double> n n (fun x y -> if x = y then 1.0 else 0.0)
  let pivotize (a : double[,]) : double[,] =
    let n = a.[*,0].Length
    let mutable p = eye n
    for i = 0 to n-1 do
      let mutable max_j = i
      for j = i to n-1 do
        if abs a.[j,i] > abs a.[max_j,i] then max_j <- j
        if max_j <> i then 
          let tmp = p.[i,*]
          p.[i,*] <- p.[max_j,*]
          p.[max_j,*] <- tmp
    p

  let LUDecompose (a : double[,]) =
    let n = a.[*,0].Length
    let p = pivotize a
    let aprime = matmul p a
    let mutable l = eye n
    let mutable u = Array2D.zeroCreate n n
    for j = 0 to n-1 do
      for i = j to n-1 do
        let mutable sum = 0.0
        for k = 0 to j do
          sum <- sum + l.[i,k] * u.[k,j]
        l.[i,j] <- aprime.[i,j] - sum
      for i = j to n-1 do
        let mutable sum = 0.0
        for k = 0 to j do
          sum <- sum + l.[j,k] * u.[k,i]
        u.[j,i] <- (aprime.[j,i] - sum) / l.[j,j]
    { P=p; A=a; L=l; U=u }

  let LUSolve (tosolve : PALU) (b : double[]) : double[] = 
    let n = tosolve.A.[0,*].Length
    let bprime = matvecmul tosolve.P b
    let mutable y = Array.zeroCreate n
    let mutable x = Array.zeroCreate n

    y.[0] <- b.[0] / tosolve.L.[0,0]
    for i = 1 to n-1 do
      let mutable sum = 0.0
      for j = 0 to i - 1 do
        sum <- sum + tosolve.L.[i,j] * y.[j]
      y.[i] <- (1.0 / tosolve.L.[i,i]) * (bprime.[i] - sum)

    x.[n-1] <- y.[n-1] / tosolve.U.[n-1,n-1]
    for i = n-2 downto 0 do
      let mutable sum = 0.0
      for j = i + 1 to n - 1 do
        sum <- sum + tosolve.U.[i,j] * x.[j]
      x.[i] <- (1.0 / tosolve.U.[i,i]) * (y.[i] - sum)
    x

  let LUimprove (tosolve : PALU) (b : double[]) (x : double[]) : double[] = 
    let n = tosolve.A.[0,*].Length
    let mutable r = Array.zeroCreate n
    for i = 0 to n-1 do
      let mutable sdp = -b.[i]
      for j = 0 to n-1 do
        sdp <- sdp + tosolve.A.[i,j] * x.[j]
      r.[i] <- sdp
    r <- LUSolve tosolve r
    Array.map2 (-) x r