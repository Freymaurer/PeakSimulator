#nowarn "10001"
namespace PeakSimulator

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///

module RealDataValues =

    module AuxFunctions =

        module AuxRealDataValues =
             
            open Deedle
            open FSharpAux
            open System.IO
            
            let wantedColumns = [|"N14Params";"N15Params"|] |> Set.ofArray
            
            let allFilesAcclimation = 
                [|yield Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\Acclimation_3h\quantFiles"); 
                  yield Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\Acclimation_15m\quantFiles");
                  yield Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\Acclimation_day2\quantFiles");
                  yield Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\Acclimation_day4\quantFiles");
                  |]
                |> Array.concat
            
            let allFilesControl =
                Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\control\quantFiles")
            
            let allFilesDeAcclimation =
                [|yield Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\De-acclimation_3h\quantFiles"); 
                  yield Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\De-acclimation_15m\quantFiles");
                  yield Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\De-acclimation_day2\quantFiles");
                  yield Directory.GetFiles(@"C:\Users\Freym\source\repos\CSBiology\CsbScaffold\PeakSimulation\data\MassSpecRealData_ForPeakSimulation\De-acclimation_day4\quantFiles");
                  |]
                |> Array.concat
            
            let allFilesCombined = 
                [|allFilesAcclimation; allFilesControl; allFilesDeAcclimation|]
                |> Array.concat
            
            //let read = Frame.ReadCsv (@"C:\Users\Freym\Desktop\RAW\Acclimation_3h\quantFiles\181005_cold2_3h_GD2_01_8996.quant",separators="\t")
            let readFrom (path:string) = Frame.ReadCsv (path,separators="\t")
            
            type Gaussian = {
                Mean        : float
                Sigma       : float
                Intensity   : float
                }
            
            type EMGaussian = {
                Mean        : float
                Sigma       : float
                Intensity   : float
                Tau         : float
                }
            
            let createGaussian mean sigma intensity = {
                Mean = mean
                Sigma = sigma
                Intensity = intensity
                }
            
            let createEMGaussian mean sigma intensity tau = {
                Mean = mean
                Sigma = sigma
                Intensity = intensity
                Tau = tau
                }

    open AuxFunctions.AuxRealDataValues
    open Deedle
    open FSharpAux

    let (realDataParameter: (int * float [] []) [] [] ) =
        allFilesCombined
        |> Array.map (fun x -> readFrom x
                               |> Frame.filterCols (fun c _ -> wantedColumns.Contains c)
                               |> Frame.mapCols (fun _ os -> os.As<string>() |> Series.map (fun _ v -> String.split ';' v)
                                                )
                               |> fun x -> Frame.getCols x
                               |> fun x -> x
                               |> Series.values
                               |> Seq.map Series.values
                               |> Seq.concat
                               |> Array.ofSeq
                               |> Array.map (fun x -> Array.filter (fun str -> str <> "" && str <> " ") x
                                                       |> Array.map (fun str -> float (str.Trim()))
                                            )
                               |> Array.groupBy (fun x -> x.Length)
                     )
    
    let getEMGaussians (parameter: (int * float [] []) []) =
        parameter
        |> Array.filter (fun (id,x) -> id = 4)
        |> Array.collect (fun (id,x) -> x)
        |> fun x -> x
        |> Array.map (fun paramArr -> createEMGaussian paramArr.[1] paramArr.[2] paramArr.[0] paramArr.[3])
        |> Array.filter (fun x -> x.Intensity >= 0.)
    
    
    let getGaussians (parameter: (int * float [] []) []) =
        parameter
        |> Array.filter (fun (id,x) -> id = 3)
        |> Array.collect (fun (id,x) -> x)
        |> fun x -> x
        |> Array.map (fun paramArr -> createGaussian paramArr.[1] paramArr.[2] paramArr.[0])
        |> Array.filter (fun x -> x.Intensity >= 0.)
    