module Utils
  let printm (a : double[,]) =
    let n = a.[*,0].Length
    let m = a.[0,*].Length
    for i = 0 to n-1 do
      for j = 0 to m-1 do
        printf "%f " a.[i,j]
      printfn ""
  let matmul (a: double[,]) (b: double[,]) : double[,] =
    let n = a.[*,0].Length
    let m = a.[0,*].Length
    let m2 = b.[0,*].Length
    Array2D.init n m2 (fun x y -> 
      let mutable s = 0.0
      for i = 0 to m-1 do
        s <- s + a.[x, i] * b.[i, y]
      s
    )
  let matvecmul (a: double[,]) (b: double[]) : double[] =
    let n = a.[*,0].Length
    let m = a.[0,*].Length
    let m2 = b.Length
    Array.init m2 (fun x ->
      let mutable s = 0.0
      for i = 0 to m-1 do
        s <- s + a.[x,i] * b.[i]
      s
    )
  let randmat (n : int) : double [,] =
    let rnd = System.Random()
    Array2D.init<double> n n (fun _ _ -> rnd.NextDouble () * 10.0)
  let randvec (n : int) : double [] =
    let rnd = System.Random()
    Array.init<double> n (fun _ -> rnd.NextDouble () * 100.0)