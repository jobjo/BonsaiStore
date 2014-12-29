namespace FSharp.BonsaiStore.Tests

module Generators =
    
    open FsCheck
    open FSharp.BonsaiStore.Internal.Filter
    open FSharp.BonsaiStore.Internal.Path

    let go :  Path = Path.Go
    let stop : Path = Path.Stop
    let step ft bs : Path = Step {Level = ft; BranchSelector = bs}


    let nextLevel = function
        | 0     -> 1
        | 1     -> 2
        | _     -> 0


    let rec pathGen level : Gen<Path> =
        let nextLevel = nextLevel level
        match level with
        | 2      ->
            Gen.elements [go; stop]
        | _                     ->
            Gen.oneof [
                Gen.constant go
                Gen.constant stop
                Gen.map (step level) <| branchSelectorGen nextLevel
                Gen.map (step level) <| branchSelectorGen nextLevel
            ]

    and branchSelectorGen level : Gen<BranchSelector> =
        let nextLevel = nextLevel level
        let selectList f t : Gen<list<int>> = 
            gen {
                let! n = Gen.choose (0, (t - f))
                return! List.replicate n (Gen.choose (f,t)) |> Gen.sequence
            }
        let withList g xs =
            xs
            |> List.map (fun d ->
                gen {
                    let! p = g
                    return (d,p)
                }
            )
            |> Gen.sequence

        let l,h =
            match level with
            | 0 -> 1850, 2150
            | 1 -> 1, 15
            | _ -> 1, 45

        let includeG  : Gen<BranchSelector> =
            gen {
                let! ks = selectList l h
                return! Gen.map Include <| withList (pathGen nextLevel) ks
            }
        let fromG =
            gen {
                let! k = Gen.choose (l,h)
                let! p = pathGen nextLevel 
                return From (k,p)
            }
        let toG =
            gen {
                let! k = Gen.choose (l,h)
                let! p = pathGen nextLevel 
                return To (k,p)
            }

        let rangeG =
            gen {
                let! k1 = Gen.choose (l,h)
                let! k2 = Gen.choose (l,h)
                let kl,kh = min k1 k2, max k1 k2
                let! p = pathGen nextLevel 
                return Range (kl,kh,p)
            }

        let customG =
            gen {
                let! f = Arb.generate<int -> bool>
                let! p = pathGen nextLevel
                return Custom <| fun k -> 
                    if f k then p else Stop
            }

        Gen.frequency [
            1, includeG
            1, fromG
            1, toG
            1, rangeG
            1, customG
        ]

    let filterGen =
        let propertyGen =
            gen {
                let! ft = 
                    Gen.oneof [
                        Gen.constant 0
                        Gen.constant 1
                        Gen.constant 2
                    ]
                let! value =
                    match ft with
                    | Year  -> Gen.choose (1990,2020)
                    | Month -> Gen.choose (1,12)
                    | Day   -> Gen.choose (1,31)

                let! op = Gen.oneof <| List.map Gen.constant [EQ; LET; GET]
                return property op ft value
            }
        
        let leafPatternGen =
            [
                1, Gen.constant True
                1, Gen.constant False
                5, propertyGen
            ]
            |> Gen.frequency
        let rec pattern n =
            if n <= 0 then
                leafPatternGen
            else
                let binary () =
                    gen {
                        let! c1 = pattern (n / 2)
                        let! c2 = pattern (n / 2)
                        let! op =
                            Gen.frequency [
                                3, Gen.constant And
                                1, Gen.constant Or
                            ]
                        return op (c1,c2)
                    }
                let unary () = Gen.map Not <| pattern (n - 1)
                gen {
                    let! p = Gen.oneof <| List.map Gen.constant [binary; unary]
                    return! p ()
                }
        Gen.sized pattern

    let seed = Random.newSeed ()

    /// Custom generators.
    type CustomGenerators =
        static member Filter() = Arb.fromGen filterGen
        static member Path() = 
            {new Arbitrary<Path>() with
                override x.Generator = (pathGen 0)
                override x.Shrinker path =
                    match path with
                    | Path.Go -> 
                        []
                    | Path.Stop -> 
                        []
                    | Path.Step step ->
                        match step.BranchSelector with
                        | BranchSelector.From (k,p) -> [p]
                        | To (k,p)                  -> [p]
                        | Range(_,_,p)              -> [p]
                        | _                         -> []
                    |> Seq.ofList
            }

    