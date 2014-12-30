Bonsai Store - F# based reporting
==============================================================
*Bonsai Store* is a project with the purpose of providing efficient reporting over sets of items of user defined data types, via an F# interface. A *Bonsai Store* can be constructed programmatically over any data type. If the data type contains indexes (annotated member methods), a tree-based (hence the name)data structure is constructed internally, enabling efficient filtering.

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
The motivation behind the project is to provide a simple interface for constructing aggregation based reports while maintaining good performance. 
Instead of using an external query language (such as `SQL`), queries are constructed in `F#` and the data is stored in memory.

The following features are emphasized:
* Simple semantics - A BonsaiStore is conceptually just a set of items.
* Reports may be constructed by composing well known operations over sequences such as  `filter`,   `map` and `fold`.
* Queries are statically type checked.
* Reusability - Query templates can be extracted and packaged as libraries.

*Bonsai Store* is particularly suited for scenarios where you have a large enough data set to make required filtering expensive and small enough to fit in memory. Between 1 and 100 million data points with a diverse set of filtering criteria would be a good candidate.

## An example ##

To give a concrete example of how *Bonsai Store* can be used, consider the following data types:

```fsharp
    open FSharp.BonsaiStore
    
    type Employee = { EmployeeId : int ; Name : string; Team : string }
    type Product = { ProductId : int; Category : string ; Name : string }
    type SalesItem = 
        {
            Product : Product
            Quantity : int
            Price : float
            Date : System.DateTime
            Employee : Employee 
        }
        [<Index; Level(0)>]
        member private this.DateIx() = 
            let span = this.Date - DateTime(1900,1,1)
            span.Days / 100

        [<Index; Level(1)>]
        member private this.PriceIx() = 
            int (this.Price / 300.)

        [<Index; Level(2)>]
        member private this.PersonNameIx() = 
            this.Employee.Name.GetHashCode() % 100

        [<Index; Level(3)>]
        member private this.CategoryIx() = 
            this.Product.Category.ToString().GetHashCode()
```

The `SalesItem` type represents an sales event and contains some basic information such as the employee responsible for the sale, the price, quantity and product.

The type is also enhanced with some private methods annotated with an `Index` attribute. These methods define the indexes to be used when constructing a bonsai store over the data type. For instance the `DateIx` method signals that items should be partitioned into groups where each items in a group are at most 100 days apart considering their `Date` property. The `Level(0)` flag means that this condition will constitute the top level in a hierarchical set of partitions. The second level will split on price intervals of 300.

Given a list of *sales items*, a *Bonsai Store* can be constructed:

```fsharp
module SB = FSharp.BonsaiStore.StoreBuilder
let store = SB.buildDefaultStore items
```
The `SB.buildDefaultStore` examines the index structure and builds a hierarchal tree where each level corresponds to an index (and the order defined by the level attribute if given). 

Reports may be defined in a *map-reduce* fashion. Here's a very simple example counting the total number of elements given an arbitrary filter:

```fsharp
module R = FSharp.BonsaiStore.Reporting
let totalNumSalesItems filter = R.report store filter 0 (fun _ -> 1) Array.sum
```
Here's the type of the `report` function:

```fsharp
val report<'T,'R>  (store: IStore<'T>) 
                   (filter: Expr<'T -> bool>) 
                   (empty: 'R) 
                   (map: 'T -> 'R) 
                   (reduce: 'R [] -> 'R) 
                   : 'R =

```
The first argument is a store object (any type implementing the `IStore` interface will do). 

The second argument is a filter condition in the form of an `Expr<'T -> bool>`.The reason for using an expression rather rather than a plain function is that expressions can be dissected and mapped to existing indexes. From a users perspective it's sufficient to think of it as a simple predicate. 

The next argument is the identity element of a report. Strictly speaking, any report defined using the `report` function must form a *commutative monoid*; That is an algebraic structure consisting of a binary operation which is commutative (i.e. changing the order of the arguments does not change the results), along with an identity element. Luckily this includes a large set of useful reports (numbers, tables, unordered lists etc).

The third argument is a `map` function constructing a report from a single item. 

The last parameter is the a *reduce* function folding an array of reports into a single report.



Here's an example of simply counting the elements, given an arbitrary filter:




`totalNumSalesItems` can now be used to count the elements using any filter condition. Here is an example of counting the number of items with price greater than 100 during the year 2010:

```fsharp
let res =
    totalNumSalesItems <@ fun item ->  item.Price > 100. && item.Date.Year = 2010 @>
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
