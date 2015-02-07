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
The objective of the project is to provide a simple interface for constructing aggregation based reports while maintaining good performance. 
Instead of using an external query language (such as `SQL`), queries are constructed in `F#` and data is stored in memory.

The following features are emphasized:
* Simple semantics - A BonsaiStore is conceptually just a set of items.
* Reports may be constructed by composing well known operations over sequences such as  `filter`,   `map` and `fold`.
* Queries are statically type checked.
* Support for abstractions - Query templates can be extracted and packaged as libraries.

*Bonsai Store* may be applied in scenarios with a large enough data set to make required filtering expensive and small enough to fit in memory. Between 1 and 100 million data points with a diverse set of filtering criteria would be a good candidate.

## An example ##

To give a concrete example of how *Bonsai Store* can be used, consider the following data types representing some aspects of a sales record:

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

The `SalesItem` type contains basic information such as the employee responsible for the sale, the price, quantity and product being sold.

The type is also enhanced with some private methods annotated with an `Index` attribute. These methods define the indexes to be used when constructing a bonsai store over the data type. For instance the `DateIx` method signals that items should be partitioned into groups where items in the same group are at most 100 days apart with respect to their `Date` properties. The `Level(0)` flag means that this condition will constitute the top level in a hierarchical set of partitions. The second level will split on price intervals of 300. The next levels will branch on name of the sales person and product category.

#### Constructing BonsaiStores
Given a list of *sales items*, a *Bonsai Store* can be constructed:

```fsharp
module SB = FSharp.BonsaiStore.StoreBuilder
let store = SB.buildDefaultStore items
```
The `SB.buildDefaultStore` examines the index structure and builds a hierarchal tree where each level corresponds to an index.

#### Basic reporting
When applicable, reports are preferably defined in a *map-reduce* fashion. Following is an example for counting the total number of elements given an arbitrary filter:

```fsharp
module R = FSharp.BonsaiStore.Reporting
let totalNumSalesItems filter = R.report store filter (fun _ -> 1) Array.sum
```

`totalNumSalesItems` can now be used to count the elements using any filtering condition. Here is an example of counting the number of items with price greater than 100 during the year 2010:

```fsharp
let res =
    totalNumSalesItems <@ fun item ->  item.Price > 100. && item.Date.Year = 2010 @>
```
The function `report` has signature:

```fsharp
val report<'T,'R>  (store: IStore<'T>) 
                   (filter: Expr<'T -> bool>) 
                   (map: 'T -> 'R) 
                   (reduce: 'R [] -> 'R) 
                   : 'R =
```

The first argument is a store object; Any type implementing the `IStore` interface will do but in the following sections I'll assume that we're using *Bonsai Stores*.

The second argument is a filter condition in the form of an `Expr<'T -> bool>`.The reason for requiring an expression rather than a plain function is that expressions can be dissected and mapped to existing indexes. This enables Bonsai Stores to slice the internal tree structure efficiently. From a users perspective it's sufficient to think of it as a simple predicate. Expressions are easy to create in F# using the quotation mechanism (simply embed the code inside a <@ and @>.

The third argument is a `map` function for constructing a report from a single item. In the example above the function maps every element to the value 1. 

The last parameter is the a *reduce* function, folding an array of reports into a single report. In the element count example, `Array.sum` transforms an array of numbers by summing all elements.

An `IStore` object is isomorphic to an unordered sequence and the semantics of `Report` can be defined as a composition of the standard `filter` and `map` operations on sequences. Here's an equivalent (but less efficient) definition of `report`:

```fsharp
let report store filter map reduce = 
    let pred = compile filter
    R.find store <@ fun _ -> true @>
    |> Array.filter pred
    |> Array.map map 
    |> reduce
```
The filter expression is compiled to a predicate function and the items are simply extracted from the store, filtered, mapped and reduced using well known constructs.

In order for the semantics of report generation to be consistent, the type of a report (`'R` in the signature above) needs to form a *commutative monoid*; That is an algebraic structure with a binary operation which is commutative and associative, along with an *identity element*. 

Given a report type `R`, with identity `id` and operator `*`, the following conditions must hold:

```fsharp
a * id = a , for every a in 'R (identity)
a * b = b * a , for every a,b in 'R (commutativity)
a * (b * c) = (a * b) * c, for every a,b,c in 'R (associativity)
```
The identity element and the binary operator are both encoded in *reduce*:

```fsharp
id = reduce [||]
reduce = Array.fold (*) id
```
The signature of reduce also enables optimized implementations of folding a sequence of elements.

Considering the example above: `R.report store filter 0 (fun _ -> 1) Array.sum`

The reduce function is `Array.sum` which can be rewritten into a fold:

```fsharp
Array.sum xs = Array.fold (+) 0
```
unveiling the corresponding `id` and (*) operator. We also need to verify  that the conditions for commutativity, associativity and identity are fulfilled:

```fsharp
a + 0 = a
a + b = b + a
a + (b + c) = (a + b) + c
```
There are many other useful report types that meet the required criteria. A few includes, numbers, tables and unordered lists.

Let's consider another example defining a report that lists the total value of sold items per team. In this case we're not aggregating values into a single number but a list of records. Here a module `ReportTypes.Table` is assumed.

```fsharp
module T = ReportTypes.Table

let topTeams filter  =
    let t = R.report store filter (fun item -> T.fromSeq [item. item]) T.merge
    T.sortBy (fun item -> ...) t
```
As in the previous example, the function is parametrized by an arbitrary filter expression. The *map* function constructs a table from a single elements and reduce merges a sequence of tables using `merge` from `ReportTypes.Table`.

Here's and example instantiating `topTeams` with a filter:

```fsharp
let topTeamsRes =
    topTeams salesStore <@ fun item ->  item.Date > DateTime(2010,7,1) && item.Price > 200 && item.Price < 500 @>
```

## Next steps
The current interface support the construction of immutable data-stores. A service interfaces for propagating changes between multiple clients will be introduced.
