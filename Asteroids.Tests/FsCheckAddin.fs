namespace FsCheck.NUnit.Examples

open NUnit.Core.Extensibility

open FsCheck.NUnit
open FsCheck.NUnit.Addin

//Not sure if this is required, but this was added by one of the nuget packages. 
//Remove if you feel like it! Probably be fine....
[<NUnitAddin(Description = "FsCheck addin")>]
type FsCheckAddin() =        
    interface IAddin with
        override x.Install host = 
            let tcBuilder = new FsCheckTestCaseBuider()
            host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
            true
