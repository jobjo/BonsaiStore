Bonsai Store - F# based reporting
==============================================================
*Bonsai Store* is a project with the purpose of providing efficient reporting over sets of items of user defined data types, via an F# interface. A *Bonsai Store* can be constructed programmatically over any data type. If the data type contains indexes (annotated member methods), a tree-based (hence the name)data structure is constructed internally, allowing for efficient filtering.

## Build ##
[![Build status](https://ci.appveyor.com/api/projects/status/6m12l77v5pn7re4e/branch/master?svg=true)](https://ci.appveyor.com/project/jobjo/bonsaistore/branch/master)

Unix:
```
$ sh build.sh All
```
Windows:
```
$ build.cmd All
```

## Rationale  ##

The motivation behind the project is to provide a simple interface for constructing aggregation based reports while maintaining good performance. Comparing with technologies such as *SQL* and *MDX/DAX (Microsoft Analysis Services)*, some features of *Bonsai Store* worth mentioning are:

* Simple semantics - Filters are expressed as predicate funtions.
* Reports may be constructed by composing well-known sequence operations such as `maps` and `folds`.
* Static type checking.
* Reusability - Templates for queries can be extracted and packaged as libraries. 

Reports may be defined in a *map-reduce* fashion. The report function also accepts a filter condition in the form of an *F# quotation* expression. The rationale behind using [F# quotations](http://msdn.microsoft.com/en-us/library/dd233212.aspx) is explained in the implementation section below.

*Bonsai Store* is particularly suited for scenarios where you have a large enough data set to make required filtering expensive and small enough to fit in memory. Between 10 and 100 million data points with a diverse set of filtering criteria would be a good candidate.

In the following sections I give a short introduction by using an example and briefly discuss the underlaying idea.

## A simple example ##

To give a concrete example of how *Bonsai Store* can be used, consider the following data types:

```fsharp
    open FSharp.BonsaiStore
    
    type Person = { Name : string ; Team : string }

    type Product = { Category : string ; Name : string }

    type SalesItem =
        {
            Product : Product
            Quantity : int
            Price : float
            Date : System.DateTime
            Person : Person
        }
        [<Index; Level(0)>]
        member private this.TeamIx() = this.Person.Team.GetHashCode()

        [<Index; Level(1)>]
        member private this.DayIx() = this.Date.Day
        
        [<Index; Level(2)>]
        member private this.MonthIx() = this.Date.Month
        
        [<Index; Level(3)>]
        member private this.YearIx() = this.Date.Year

        [<Index; Level(4)>]
        member private this.PersonNameIx() = this.Person.Name.GetHashCode()

        [<Index; Level(5)>]
        member private this.PriceIx() = int (this.Price / 100.)
```

Given a list of *sales items*, a *Bonsai Store* can be constructed:

```fsharp
module SB = FSharp.BonsaiStore.StoreBuilder
let store = SB.buildStore<SalesItem, int> salesItems
```
Reports are then defined in a *map-reduce* style. Here's an example of simply counting the elements, given an arbitrary filter:

```fsharp
let totalNumSalesItems (salesStore: IBonsaiStore<SalesItem,int>) filter =
    SB.report salesStore filter 0 (fun _ -> 1) Seq.sum
```

`totalNumSalesItems` can now be used to count the elements using any filter condition. Here is an example of counting the number of items with price over 100 during the year 2010:

```fsharp
let res =
    totalNumSalesItems salesStore <@ fun item ->  item.Price > 100. && item.Date.Year = 2010 @>
```
Note that the filter expression is giveen as an F# quotation (code snipptes surrounded by <@ and @>). The reason why a quotation is required is that it can be deconstructed and transformed into a more efficient instruction for how to select elements from the store.

Here is another example of a slightly more interesting table based report template:

```fsharp
let tableReport pick salesStore filter  =
    SB.report salesStore filter Table.empty (fun item -> Table.fromSeq [pick item]) Table.merge
```
`tableReport` accepts an arbitrary function for selecting a key-value pair from an item and will generate a table aggregating the values with identical keys. The function may be used for defining a *top teams* reporting template.

```fsharp
let topTeams =
    tableReport (fun item -> item.Person.Team, float item.Quantity * item.Price)
```
Finally the `topTeams` report may be instantiated with a filter:

```fsharp
let topTeamsRes =
    topTeams salesStore <@ fun item ->  
        item.Date.Year = 2011  && item.Date.Month = 7 @>
```

##  A note on implementation ###
The foundation of *Bonsai Store* is a prefix-tree data structure, where each level of the tree splits the items based on a particular property (index.) In the example above the hierarchies include *product*, *years* and *months*. When querying the underlying *tree* with a filter looking at for example a particular *product* in a given *year*, only a subset of the children are considered at each level. This allows a large set of items to be discarded at once (improving on linear complexity). Each node of the tree also allow efficient filtering based on ranges, for instance filtering out items with prices below a certain threshold).

## Next steps
The current interface support the construction of immutable data-stores. A service interfaces for propagating changes between multiple clients will be added. The map-reduce interface will be re-evaluated. Quotation translation will be improved.
