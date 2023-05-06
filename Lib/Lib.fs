namespace Dormant


open Form.Attributes


// type ColumnAlias = ( string * string )

// type Table< ^T >( dbContext, ( constraints : Seq<string * Seq<Constraints>> ), fks[] ) = 
//     // let mutable data = Seq.empty< ^T >
//     // ^T.GetProperties() 
//     // |> attach constrains/fks to properties
//     // |> fold 
//     let lookup t1 t2 = 
//         t1[ getRelation t1 t2 ]
//     let map f = 
//         Seq.map f data
    
//     let selectAll () =
//         Orm.SelectAll< ^T >
        
//     let get id : Result< ^T, exn > = Seq.filter  ( fun x -> x.getpks )

// type Constraints = 
//     | Unique 
//     | Default of string

// type SomethingElse = 
//     {
//         id: int
//         id2: int
//     }
// type SomethingElse2 = 
//     {
//         id: int
//     }

// type Row = 
//     {
//         id: int 
//         name: string
//         somethingElse: SomethingElse 
//         someThingElse2: SomethingElse2
//         aConstraint: DateTime
//     }

// type Relationship< ^T > ( cols: Column< ^T > seq )  = //SomethingElse, ["id", "id2"]


// type Column< ^T > ( name: string ) = 
//     typedefof<^T>. GetProperties(name) if null then throw "Cannot find column on that type"

type ComparisonOperation = 
    | Is // is
    | IsNot // is not
    | Equals // =
    | NotEquals // <>
    | LessThan // <
    | GreaterThan // >
    | LessThanOrEqualTo // <=
    | GreaterThanOrEqualTo // >=

type Comparison< ^T > = ( Column * ComparisonOperation * ^T ) 

type Disjunct< ^T > = ( Column * bool * ^T ) //col1 [NOT] IN (1,2,3,4,5) 

type Conjunct = 
    | Comparison of Comparison< ^T >
    | Disjunct of Disjunct< ^T >

type FilterPredicate = Conjunct seq // e.g. [Comparison (comparison1); Disjunct (disjunct1); Comparison (comparison2)] -> "WHERE comparison1 AND disjunct1 AND comparison2"

type Direction = 
    | Ascending
    | Descending

type IndexType = 
    | Clustered
    | NonClustered

type DataCompressionType = 
    | Row
    | Page

type Partition = 
    | Single of int 
    | Range of int * int

type RelationalIndexOptions = 
    | PadIndex
    | FillFactor of fillfactor
    | SortInTempDb
    | IgnoreDupKey
    | StatisticsNorecompute
    | DropExisting
    | Online // = { ON [ ( <low_priority_lock_wait> ) ] | OFF }
    | Resumable
    | MaxDuration // = <time> [MINUTES]
    | AllowRowLocks
    | AllowPageLocks
    | OptimizeForSequentialKey
    | MaxDOP of int
    | DataCompression of ( DataCompressionType * Partition ) seq  //Some [ Row, Range (1,2); Page, Single 1; Page, Range (10,15)] || None 
    | XMLCompression of Partition seq 

type Index = ( bool * IndexType * string * ( Column * Direction ) seq * Column seq * FilterPredicate * )

(*
{
    PAD_INDEX = { ON | OFF }
  | FILLFACTOR = fillfactor
  | SORT_IN_TEMPDB = { ON | OFF }
  | IGNORE_DUP_KEY = { ON | OFF }
  | STATISTICS_NORECOMPUTE = { ON | OFF }
  | STATISTICS_INCREMENTAL = { ON | OFF }
  | DROP_EXISTING = { ON | OFF }
  | ONLINE = { ON [ ( <low_priority_lock_wait> ) ] | OFF }
  | RESUMABLE = { ON | OFF }
  | MAX_DURATION = <time> [MINUTES]
  | ALLOW_ROW_LOCKS = { ON | OFF }
  | ALLOW_PAGE_LOCKS = { ON | OFF }
  | OPTIMIZE_FOR_SEQUENTIAL_KEY = { ON | OFF }
  | MAXDOP = max_degree_of_parallelism
  | DATA_COMPRESSION = { NONE | ROW | PAGE }
     [ ON PARTITIONS ( { <partition_number_expression> | <range> }
     [ , ...n ] ) ]
  | XML_COMPRESSION = { ON | OFF }
     [ ON PARTITIONS ( { <partition_number_expression> | <range> }
     [ , ...n ] ) ]
}
*)


(*
CREATE NONCLUSTERED INDEX IX_TransactionHistory_ReferenceOrderID
  ON Production.TransactionHistory (ReferenceOrderID Desc)
  INCLUDE (AddressLine1, AddressLine2, City, StateProvinceID)
  WHERE EndDate IS NOT NULL
  WITH (
    DATA_COMPRESSION = PAGE ON PARTITIONS(1),
    DATA_COMPRESSION = ROW ON PARTITIONS (2 TO 4)
  )
  ON TransactionsPS1 (TransactionDate);
*)



type Table = ( string * Column seq * Index seq )


and Column ( name: string, _type: Type, constraints: Constraint seq ) = // {name = ""; _type = SomeType; constraints = [...]} || Column("aCol", SomeType, [...])
    member _.name = name
    member _._type = _type
    member _.constraints = constraints 
    
and Constraint = 
    | Unique 
    | Default of string
    | Null
    | NotNull
    | Check of string //Check "col1 > 0"
    | PrimaryKey
    | ForeignKey of Table * Column

    
    // Table (
    //     seq {
    //     [ Column (1, int); Column(2; int) Column ("bignig", string)]
    //     }
    // )

let col = Column ( "a", string.GetType() )

col.name

type Table = Column seq
     let makeEntity vals = FSharpValue.MakeRecord( rty, vals )

let SomethingTable = [ Column ("id", int); Column("yamotha"; int) Column ("bignig", string)]



let inline generateReader< ^T > ( reader : IDataReader )  ( this : OrmState ) = 
        let rty = typeof< ^T >
        let makeEntity vals = FSharpValue.MakeRecord( rty, vals ) :?>  ^T
        let fields = 
            seq { for fld in ( columnMapping< ^T > this ) -> fld.SqlName, fld } 
            |> dict 
        seq { 
            while reader.Read( ) do
                yield 
                    seq { 0..reader.FieldCount-1 }
                    |> Seq.map ( fun i -> reader.GetName( i ), reader.GetValue( i ) )
                    |> Seq.sortBy ( fun ( n, _ ) ->  fields[n].Index )
                    |> Seq.map ( fun ( n, v ) -> 
                        match optionType< ^T > fields[n].Type this with
                        | Some t -> toOption< ^T > t v this
                        | None   -> v
                    )
                    |> Seq.toArray
                    |> makeEntity 

let rows = Table<Row>( [ ( "aConstraint", [ Unique; Default "now()" ] ) ] 
                    ,  [ Relationship<SomethingElse> ( 
                            [ 
                                Column<SomethingElse>("id") 
                                Column<SomethingElse>("id2") ] 
                            ) 
                        ]) 
    
    type Animal = 
        {
            this : string 
            [<Column("this", Context)>]
            that : string
        }
    let rows = Table<Animal>( ["aConstraint unique"], [ [ "fk1", "fk2" ], [ "fk3" ] ], [ "id" ] ) 
    
    getAnimal : int -> Table<Animal>
    

    getAnimal = fun id -> async{
        selectWhere $"id = {id}" rows 
        // rows.selectWhere $"id = {id}"
        return rows 
    }
    getAnimals = fun () -> async{
        rows.selectAll ()
        return rows 
    }
rows.selectWhere () 