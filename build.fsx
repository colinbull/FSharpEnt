#r "tools/Fake/tools/FakeLib.dll"
#I "packages/FSharp.Formatting.1.0.13/lib/net40"
#load "packages/FSharp.Formatting.1.0.13/literate/literate.fsx"

open Fake
open System.IO
open FSharp.Literate

let nugetPath = Path.Combine(__SOURCE_DIRECTORY__,@"tools\NuGet\NuGet.exe")


let projectName, version = "FSharp.Enterprise",  if isLocalBuild then ReadFileAsString "local_build_number.txt" else tcBuildNumber

let buildDir, testDir, deployDir, docsDir, nugetDir = @"build\artifacts", @"build\test", @"build\deploy", @"build\docs", @"build\nuget"
let nugetDocsDir = nugetDir @@ "docs"
let nugetKey = if System.IO.File.Exists "./nuget-key.txt" then ReadFileAsString "./nuget-key.txt" else ""

let appReferences = !! @"src\**\*.*sproj"
let testReferences = !! @"tests\**\*.Tests.*sproj"

Target "RestorePackages" RestorePackages

Target "Clean" (fun _ -> 
    CleanDirs [buildDir; testDir; deployDir; docsDir]
)

Target "AssemblyInfo" (fun _ -> 

        AssemblyInfo (fun p -> 
            { p with 
                CodeLanguage = FSharp
                AssemblyVersion = version
                AssemblyTitle = projectName
                Guid = "207C7E5B-DFFF-41DC-849A-53D10A0FF644"
                OutputFileName = "src/FSharp.Enterprise/AssemblyInfo.fs"                
            })
        AssemblyInfo (fun p -> 
            { p with 
                CodeLanguage = FSharp
                AssemblyVersion = version
                AssemblyTitle = projectName + ".RabbitMq"
                Guid = "D93F9436-D1DD-4FB1-9C0A-52298F9F0215"
                OutputFileName = "src/FSharp.Enterprise.RabbitMq/AssemblyInfo.fs"                
            })
        AssemblyInfo (fun p -> 
                  { p with 
                      CodeLanguage = FSharp
                      AssemblyVersion = version
                      AssemblyTitle = projectName + ".Web"
                      Guid = "C4855501-6D39-44CF-B55E-DE8EE16516AC"
                      OutputFileName = "src/FSharp.Enterprise.Web/AssemblyInfo.fs"                
                  })

)

Target "BuildApp" (fun _ ->
    MSBuild buildDir "Build" ["Configuration","Release"; "Platform", "anycpu"] appReferences |> Log "BuildApp: "
)

Target "BuildTest" (fun _ ->
    MSBuildDebug testDir "Build" testReferences
        |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->
    !+ (testDir + "/*.Tests.dll")
        |> Scan
        |> NUnit (fun p ->
            {p with
                DisableShadowCopy = true
                OutputFile = testDir + "\TestResults.xml" })
)

Target "Docs" (fun _ -> 
    let template = Path.Combine(currentDirectory, "template.html")
    let sources = Path.Combine(__SOURCE_DIRECTORY__, "samples")
    let output = docsDir
    
    Literate.ProcessDirectory(sources, template, output)
    
    XCopy docsDir "docs"
)

Target "Deploy" (fun _ ->
    !+ (buildDir + "/**/FSharp.Enterprise*.dll")
        -- "*.zip"
        |> Scan
        |> Zip buildDir (deployDir + sprintf "\%s-%s.zip" projectName version)
)

Target "BuildNuGet" (fun _ ->
    CleanDirs [nugetDir; nugetDocsDir]
    XCopy docsDir nugetDocsDir
    printfn "%s" nugetPath
    [
        "lib", buildDir + "\FSharp.Enterprise.dll"
        "lib", buildDir + "\FSharp.Enterprise.RabbitMq.dll"
        "lib", buildDir + "\FSharp.Enterprise.Web.dll"
    ] |> Seq.iter (fun (folder, path) -> 
                    let dir = nugetDir @@ folder @@ "net40"
                    CreateDir dir
                    CopyFile dir path)
    NuGet (fun p ->
        {p with               
            Authors = ["Colin Bull"]
            Project = projectName
            Description = "F# Enterprise Library collection"
            Version = version
            OutputPath = nugetDir
            WorkingDir = nugetDir
            AccessKey = nugetKey
           // ToolPath = "tools\Nuget\Nuget.exe"
            Publish = nugetKey <> ""})
        ("./FSharp.Enterprise.nuspec")
    [
       (nugetDir) + sprintf "\FSharp.Enterprise.%s.nupkg" version
    ] |> CopyTo deployDir
)


Target "Default" DoNothing

"RestorePackages"
    ==> "Clean"
    ==> "BuildApp" <=> "BuildTest"
    ==> "Test" 
    ==> "Docs"
    ==> "BuildNuGet"
    ==> "Deploy"
    ==> "Default"
  
if not isLocalBuild then
    "Clean" ==> "SetAssemblyInfo" ==> "BuildApp" |> ignore

// start build
RunParameterTargetOrDefault "target" "Default"
