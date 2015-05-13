module ExampleTests

open NUnit.Framework
open FsUnit
open FsCheck

(*  
    References: 
    http://www.nunit.org/
    https://github.com/fsprojects/FsUnit
    https://github.com/fsharp/FsCheck

    Make sure to install the Nunit test adapter from visual studio extensions and updates to run the tests
    *)


[<Test>]
let ``The maximum of a and b is (a>b)``() = 
    let maximum a b = if a > b then a else b
    maximum 10 0 |> should equal 10
