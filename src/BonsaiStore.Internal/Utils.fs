namespace FSharp.BonsaiStore.Internal

module Utils =
    module Printer =

        /// A printer is a function from an indentation level to a list of strings.
        type Printer = private { Run : int -> list<string>}

        /// Creates a printer
        let private mkPrinter f = {Run = f}

        let private space n = Seq.fold (+) "" <| List.replicate (n * 2) " "

        let printWith space s =
            mkPrinter <| fun n -> [sprintf "%s%s" (space n) s]

        let print = printWith space

        let printFlat = printWith (fun _ -> "")

        /// Nests a printer
        let indent p = mkPrinter <| fun n -> p.Run (n+1)

        /// Runs a tree printer returning a string.
        let run (p: Printer) = 
            let sb = new System.Text.StringBuilder()
            for s in p.Run 0 do
                ignore <| sb.Append s
                ignore <| sb.Append "\n"
            sb.ToString()

        /// An empty printer.
        let empty = mkPrinter <| fun _ -> []

        /// Prints a string
        let (!) = print
    
        /// Composes two printers.
        let (<+>) tp1 tp2 = mkPrinter <| fun n -> tp1.Run n @ tp2.Run n

        /// Composes a list of printers.
        let (!<) ps = Seq.fold (<+>) empty ps

