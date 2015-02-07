namespace FSharp.BonsaiStore.Reports

module Numerical =
    open FSharp.BonsaiStore

    /// Sum values.
    let sumBy<'T> store filter (f: 'T -> float) =
        report store filter f Array.sum

    /// Retrieve average value.
    let averageBy<'T> store filter (f: 'T -> float) =
        let (count,sum) =
            report 
                store 
                filter 
                (fun x -> (1., f x))
                (Array.fold (fun (n,v) (n',v') -> (n + n',v + n' * v')) (0.,0.))
        sum / count

    /// Find maximum value.
    let maxBy<'T> store filter (f: 'T -> float) =
        report store filter f Array.max

    /// Find minimum value.
    let minBy<'T> store filter (f: 'T -> float) =
        report store filter f Array.min



