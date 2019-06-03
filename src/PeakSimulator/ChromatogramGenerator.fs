namespace PeakSimulator

open FSharpAux
open FSharp.Stats
open PeakSimulator
open RealDataValues
open FSharp.Plotly


module ChromatogramGenerator =
    module AuxFunctions =
        
        module AuxCsvWriterAndFinalizer =
    
            open SimulatePeak
            open AuxFunctions.AuxTypes
    
            type LabeledSimulatedXic = {
                    ID                      : int 
                    NoisyXicIntensity       : float []
                    XPosition               : float []
                    CodedStringArray        : string []
                    EMGaussians             : EMGaussian []
                }
            
            let createLabeledSimulatedXic (id:int) (xPos:float[]) (noisyGaus:float[]) (codedStrArr: string []) (emgausArr: EMGaussian[]) = {
                    ID                          = id 
                    XPosition                   = xPos 
                    NoisyXicIntensity           = noisyGaus 
                    CodedStringArray            = codedStrArr
                    EMGaussians                 = emgausArr
                }
            
            type CsvSimulatedXic = {
                    ID                      : int 
                    NoisyXicIntensity       : string
                    XPosition               : string
                    CodedStringArray        : string
                    EMGaussians             : string []
                }
            
            let createCsvSimulatedXic id (xPos:float[]) (noisyGaus:float[]) (codedStrArr: string[]) emgausArr = 
                let writeString floatArr = 
                    floatArr
                    |> Array.map (fun x -> string x)
                    |> Array.fold (fun elem arr -> elem + ";" + arr) ""
                    |> fun x -> x.TrimStart [|';'|]
                let writeString' strArr =
                    strArr
                    |> Array.fold (fun elem arr -> elem + ";" + arr) ""
                    |> fun x -> x.TrimStart [|';'|]
                let writeStringEMGaussians (emgaussians:EMGaussian []) =
                    emgaussians
                    |> Array.map (fun x -> sprintf "Mean=%A;Sigma=%A;Intensity=%A;Tau=%A;Mode=%A" x.Mean x.Sigma x.Intensity x.Tau x.Mode)
                {
                    ID                          = id 
                    XPosition                   = writeString xPos 
                    NoisyXicIntensity           = writeString noisyGaus 
                    CodedStringArray            = writeString' codedStrArr
                    EMGaussians                 = writeStringEMGaussians emgausArr
                }
            
            
            ///is meant to create a baseFramwork for the coded Xic array. Starts at the beginning of the XicArray
            ///and ends with its end, fills all spaces with an X (a placeholder and/or describing that position as
            ///not part of an peak
            let createBaseCodedArray (simXic:SimulatedXic) =
            
                let getXPositions = simXic.DistributionArray 
                                    |> Array.unzip
                                    |> fun (x,y) -> x
                let createBaseCode = Array.init getXPositions.Length (fun _ -> "X")
                Array.zip getXPositions createBaseCode
                
            ///This function returns the number a specific item ("item") is found in an array (tArray)
            let countItem item (tArray: 'T []) = 
                let rec countingLoop counts position =
                    if position >= (tArray.Length-1)
                        then counts
                        else match item with 
                                | item when item = tArray.[position] -> countingLoop (counts + 1) (position + 1)
                                | _ -> countingLoop counts (position + 1)
                countingLoop 0 0
            
            ///is meant to fill the BaseCodedArray with meaningful positions: Apex (A), leftLiftOff (LLO), rightLiftOff (RLO)
            ///rightEnd (RE), leftEnd (LE) and PartOfPeak (P).
            let getPointsOfInterest (simXic:SimulatedXic) (baseCodedArr:(float*string)[]) = 
                
                let addStringAtIndice (targetArr:(float*string)[]) (addition:string) (indiceArr:int[]) = 
                    indiceArr
                    |> Array.filter (fun x -> x <> -20)
                    |> fun indices -> Array.mapi (fun i (xPos,str) -> if (Array.contains i indices) = true 
                                                                      then xPos,str+addition
                                                                      else xPos,str
                                                 ) targetArr
            
                let getApex arr = simXic.PeakInformartions
                                  |> Array.map (fun x -> x.Apex.Index
                                               )
                                  |> addStringAtIndice arr "A"
            
                let getLeftLiftOff arr = simXic.PeakInformartions
                                         |> Array.map (fun x -> if x.LeftLiftOff.IsSome = true
                                                                then x.LeftLiftOff.Value.Index
                                                                else -20
                                                      )
                                         |> addStringAtIndice arr "LLO"
            
                let getLeftEnd arr = simXic.PeakInformartions
                                     |> Array.map (fun x -> x.LeftEnd.Index
                                                   )
                                     |> addStringAtIndice arr "LE"
            
                let getRightLiftOff arr = simXic.PeakInformartions
                                          |> Array.map (fun x -> if x.RightLiftOff.IsSome = true 
                                                                 then x.RightLiftOff.Value.Index
                                                                 else -20
                                                       )
                                          |> addStringAtIndice arr "RLO"
            
                let getRightEnd arr = simXic.PeakInformartions
                                      |> Array.map (fun x -> x.RightEnd.Index)
                                      |> addStringAtIndice arr "RE"
            
                let getPartOfPeak (arr:(float*string)[]) = simXic.PeakInformartions
                                                           |> Array.collect (fun x -> x.XData)
                                                           |> fun xPosArr -> arr |> Array.map (fun (xPos,str) -> xPos,str + (String.init (countItem xPos xPosArr) (fun _ -> "P") 
                                                                                                                            ) 
                                                                                              )
                baseCodedArr
                |> getApex
                |> getLeftLiftOff
                |> getRightLiftOff
                |> getLeftEnd
                |> getRightEnd
                |> getPartOfPeak
                |> Array.map snd
            
            
            ///Erase "X" from all points that are part of a peak or a point of interest. Erase "P" from all points of interest.
            let stringCleanUp (codedArr :string []) = 
                codedArr
                |> Array.map (fun str -> if str <> "X" 
                                         then str.TrimStart([|'X'|])
                                         else str
                             )
                |> Array.map (fun str -> if str <> "P" && (str.EndsWith "PP") = false
                                         then str.TrimEnd([|'P'|])
                                         else str
                             )
            
            
            let createStringCodedPeaks (simXic:SimulatedXic) =
                createBaseCodedArray simXic
                |> getPointsOfInterest simXic
                |> stringCleanUp
            
            
            /////function for image for presentation
            //let blankFrameArray = createFrameArray 0 7 0.02
            /////a = height of the gaussians peak, b = mean of gaussian distri, c = sigma of gaussian distri, x = window on x -axis
            //let gaussianFunctionForY maxIntensity mean sigma x =  maxIntensity*(exp( -((x-mean) ** 2.) / (2.*(sigma ** 2.)) ))
            //let getGaus intensity mean sigma =
            //    blankFrameArray
            //    |> Array.map (fun (x,y) -> x, gaussianFunctionForY intensity mean sigma x)
            //let getSndOfSummedUpGaus (gaussians : (float*float*float)[]) =
            
            //    gaussians
            //    |> Array.map (fun (intensity,mean,sigma) -> getGaus intensity mean sigma
            //                 )
            //    |> Array.concat
            //    |> Array.groupBy (fun (x,y) -> x)
            //    |> Array.map (fun (x,tuple) -> x, Array.sumBy (fun (x,y) -> y) tuple )
            //    |> Array.unzip
            //    |> fun (x,y) -> x, savitzky_golay 5 3 2 1 y |> Vector.toArray
            //    |> fun (x,yArr) -> x, (Array.map (fun yInt -> yInt /(Array.max yArr) ) yArr)
            //    //|> fun (x,y) -> Chart.Point (x,y, Name = "Second Derivation")
            /////functions for images end here
            
            
            
            //let this = FromRandomGenerator.createRndPeakArray   20 //number of peaks 
            //                                                    0.55 // emg propability
            //                                                    0. //mean from
            //                                                    50. //mean to
            //                                                    10000. //intensity from
            //                                                    999999. //intensity to
            //                                                    0.2 //intensity noise stdv
            //                                                    1. //sigma mean
            //                                                    0.5 //sigma stdv
            //                                                    0.8 //tau mean
            //                                                    0.2 //tau stdv
            //                                                    0.01 // window size
            //                                                    1000. //white noise mean
            //                                                    2000. //white noise stdv
            //                                                    1.5 //min distance peaks
            //                                                    1.3 //min difference intensity 
            
            //let that = FromRealData.createPeakArray    10 //Number of Peaks
            //                                           AuxFunctions.RealData.NumberOne // RealData
            //                                           0.3 //IntensityNoiseStdv
            //                                           0.01 //windowSize
            //                                           1000. //whiteNoiseMean
            //                                           4000. //whiteNoiseStdv
            //                                           2. //minDistancePeaks
            //                                           2. //minDifferenceIntensity
        


    open SimulatePeak.AuxFunctions.RealData
    open SimulatePeak.AuxFunctions.AuxTypes
    open AuxFunctions.AuxCsvWriterAndFinalizer
    open SimulatePeak.FromRandomGenerator
    open SimulatePeak.FromRealData
    open FSharpAux.IO

    ///This function returns simulated peak array.
    ///The function can create peaks based on random variables or on real data distributions. Depending on the chosen option the function takes paramArrays of different length.
    ///REALDATA: n = number of peak distributions; fromPremade = true; paramArr = NumberOfPeaks, continuousPeaks, RealData-ID (1.), IntensityNoiseStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity; filePate = @"C:\path\".
    ///RANDOMDATA: n = number of peak distributions; fromPremade = false; paramArr = NumberOfPeaks, continuousPeaks, emgPropability, meanFrom, meanTo, intensityFrom, intensityTo, 
    ///intensityNoiseStdv, sigmaMean, sigmaStdv, tauMean, tauStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity 
    let chromatogramToCSV n (fromPremade:bool) (paramArr:float []) (filePath: string)=
        match fromPremade with
        | false -> if paramArr.Length <> 17 then failwith "paramArr must have either 9 parameters for PeakSimulation from RealData distributions or 17 for random PeakSimulation!" 
        | true -> if paramArr.Length <> 9 then failwith "paramArr must have either 9 parameters for PeakSimulation from RealData distributions or 17 for random PeakSimulation!" 
        if filePath.EndsWith @"\" = false then failwith @"filepath must end with \ (backslash)"
        if n = 1 then failwith "must have at least n = 2"
        let matchRealDataId (value:float) =
            match value with
            | 1. -> NumberOne
            | _ -> failwith "Unknown RealData id, please use another paramater for realData (use: 1.)"
        let matchContinuosPeakID (value:float) =
            match value with
            | 1. -> true
            | 0. -> false
            | _ -> failwith "Value to determine continuous peak must be either 0 for non-continuous or 1 for continuous peaks"
    
        let addGausNoise (codedGaus: SimulatedXic) (gausArr:(float*float) []) =
            let yNoise =
                codedGaus.NoiseArray
                |> Array.unzip
                |> fun (x,y) -> y
            gausArr 
            |> Array.map2 (fun yNoise (x,yInt) -> x, yInt + yNoise) yNoise
    
    
        let addWhiteNoise' (codedGaus: SimulatedXic) (gausArr: (float*float)[]) =
            codedGaus.WhiteNoiseArray
            |> Array.map2 (fun (gX,gY) (wX,wY) -> gX,gY+wY) gausArr
    
        let createCodedGausWithNoises = 
            let doNTimes =
                [|0 .. n|]
            doNTimes 
            |> Array.map (fun _ -> match fromPremade with
                                   | false -> createRndPeakArray (int paramArr.[0]) (matchContinuosPeakID paramArr.[1]) paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8] paramArr.[9] paramArr.[10] paramArr.[11] paramArr.[12] paramArr.[13] paramArr.[14] paramArr.[15] paramArr.[16]
                                              |> fun x -> (*printfn "Start creating string coded peaks"*)
                                                          x, createStringCodedPeaks x
                                              ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlier.
                                              |> fun (gausInfo,codedStr) -> (*printfn "Start adding noise"*)
                                                                            (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians 
                                   | true -> createPeakArray (int paramArr.[0]) (matchContinuosPeakID paramArr.[1]) (matchRealDataId paramArr.[2]) paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8]
                                             |> fun x -> (*printfn "Start creating string coded peaks"*)
                                                         x, createStringCodedPeaks x
                                             ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlie
                                             |> fun (gausInfo,codedStr) ->  (*printfn "Start adding noise"*)
                                                                            (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians
                         )
            |> Array.mapi (fun i (noisyArr,codedStr,emgaussians) -> i,(noisyArr |> Array.map fst), (noisyArr|> Array.map snd), codedStr, emgaussians
                          )
            |> Array.map (fun (id,xPos,yNoisy,codedStr,emgaussians) -> createLabeledSimulatedXic id xPos yNoisy codedStr emgaussians)
        //here will be imaging part
            //|> fun x -> x.[0]
            //|> fun (gausInfo) -> Array.zip3 gausInfo.CodedStringArray gausInfo.XPosition gausInfo.NoisyXicIntensity 
            //|> fun arr ->   Array.filter (fun (x,y,z) -> x = "P") arr, Array.filter (fun (x,y,z) -> (String.contains "A" x )= true) arr,
            //                Array.filter (fun (x,y,z) -> (String.contains "PP" x )= true) arr, Array.filter (fun (x,y,z) -> (String.contains "LE" x ) = true || (String.contains "RE" x ) = true ) arr,
            //                Array.filter (fun (x,y,z) -> x = "X" ) arr, 
            //                Array.filter (fun (x,y,z) -> x <> "P" && x <> "X" && (String.contains "PP" x ) = false && (String.contains "A" x )= false && (String.contains "LE" x ) = false && (String.contains "RE" x ) = false) arr
            //|> fun (p,a,pp,lere,x,rest) ->   [
            //                                    (Array.unzip3 p |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Part Of Peak", Labels = str, Opacity = 0.8) ) )
            //                                    (Array.unzip3 a |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Apex", Labels = str) ) )
            //                                    (Array.unzip3 pp |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Overlap", Labels = str) ) )
            //                                    (Array.unzip3 lere |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Left End/Right End", Labels = str) ) )
            //                                    (Array.unzip3 x |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Not Part Of Peak", Labels = str, Opacity = 0.8) ) )
            //                                    (Array.unzip3 rest |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Rest", Labels = str, Opacity = 0.5) ) )
    
            //                                ]
            //|> Chart.Combine
            //|> Chart.withSize (1000.,600.)
            //|> Chart.Show
        ////here end imaging part
            //|> Array.map (fun (id,xPos,yNoisy,codedStr,emgaussians) -> createCsvSimulatedXic id xPos yNoisy codedStr emgaussians)
            |> Seq.ofArray
            |> Seq.toCSV "," true
            |> fun x -> if paramArr.Length = 17 then Seq.append [sprintf "# VersionNo. 1
                                         # NumberOfPeaks = %A; Continuous Peaks = %A; EMGPropability = %A; MeanFrom-MeanTo = %A - %A; IntensityFrom-IntensityTO = %A - %A; IntensityNoiseStdv = %A;
                                         # SigmaMean = %A; SigmaStdv = %A; TauMean = = %A; TauStdv = %A; Windowsize = %A; WhiteNoiseMean = %A; WhiteNoiseStdv = %A; Minimal distance between two peaks = %A sigma; minimal intensity difference at apex = %A" (int paramArr.[0]) paramArr.[1] paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8] paramArr.[9] paramArr.[10] paramArr.[11] paramArr.[12] paramArr.[13] paramArr.[14] paramArr.[15] paramArr.[16]
                                                                ] x
                        elif paramArr.Length = 9 then Seq.append [sprintf "# VersionNo. 1
                                          # NumberOfPeaks = %A; Continuous Peaks = %A; Origin RealDataSet for Parameters = %A; IntensityNoiseStdv = %A;
                                          # Windowsize = %A; WhiteNoiseMean = %A; WhiteNoiseStdv = %A; Minimal distance between two peaks = %A sigma; minimal intensity difference at apex = %A" (int paramArr.[0]) paramArr.[1] paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8]
                                                                 ] x
                        else failwith "ParamArr must have length 8 or 16!"
            ///takes most of the time
            |> Seq.write (filePath + (sprintf @"%A-%A-%A" System.DateTime.UtcNow.Year System.DateTime.UtcNow.Month System.DateTime.UtcNow.Day) + @"_SimulatedXics" + (String.replace ":" "-" (sprintf @"_%A" System.DateTime.UtcNow.TimeOfDay)) + @".csv" )
       
        createCodedGausWithNoises
    

    ///This function returns simulated peak array.
    ///The function can create peaks based on random variables or on real data distributions. Depending on the chosen option the function takes paramArrays of different length.
    ///REALDATA: n = number of peak distributions; fromPremade = true; paramArr = NumberOfPeaks, continuousPeaks, RealData-ID (1.), IntensityNoiseStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity; filePate = @"C:\path\".
    ///RANDOMDATA: n = number of peak distributions; fromPremade = false; paramArr = NumberOfPeaks, continuousPeaks (0. = false,1. = true), emgPropability (0-1 in percent), meanFrom, meanTo, intensityFrom, intensityTo, 
    ///intensityNoiseStdv, sigmaMean, sigmaStdv, tauMean, tauStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity 
    let chromatogramToImg n (fromPremade:bool) (paramArr:float []) =
        match fromPremade with
        | false -> if paramArr.Length <> 17 then failwith "paramArr must have either 9 parameters for PeakSimulation from RealData distributions or 17 for random PeakSimulation!" 
        | true -> if paramArr.Length <> 9 then failwith "paramArr must have either 9 parameters for PeakSimulation from RealData distributions or 17 for random PeakSimulation!" 
        if n = 1 then failwith "must have at least n = 2"
        let matchRealDataId (value:float) =
            match value with
            | 1. -> NumberOne
            | _ -> failwith "Unknown RealData id, please use another paramater for realData (use: 1.)"
        let matchContinuosPeakID (value:float) =
            match value with
            | 1. -> true
            | 0. -> false
            | _ -> failwith "Value to determine continuous peak must be either 0 for non-continuous or 1 for continuous peaks"
    
        let addGausNoise (codedGaus: SimulatedXic) (gausArr:(float*float) []) =
            let yNoise =
                codedGaus.NoiseArray
                |> Array.unzip
                |> fun (x,y) -> y
            gausArr 
            |> Array.map2 (fun yNoise (x,yInt) -> x, yInt + yNoise) yNoise
    
    
        let addWhiteNoise' (codedGaus: SimulatedXic) (gausArr: (float*float)[]) =
            codedGaus.WhiteNoiseArray
            |> Array.map2 (fun (gX,gY) (wX,wY) -> gX,gY+wY) gausArr
    
        let createCodedGausWithNoises = 
            let doNTimes =
                [|0 .. n|]
            doNTimes 
            |> Array.map (fun _ -> match fromPremade with
                                   | false -> createRndPeakArray (int paramArr.[0]) (matchContinuosPeakID paramArr.[1]) paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8] paramArr.[9] paramArr.[10] paramArr.[11] paramArr.[12] paramArr.[13] paramArr.[14] paramArr.[15] paramArr.[16]
                                              |> fun x -> (*printfn "Start creating string coded peaks"*)
                                                          x, createStringCodedPeaks x
                                              ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlier.
                                              |> fun (gausInfo,codedStr) -> (*printfn "Start adding noise"*)
                                                                            (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians 
                                   | true -> createPeakArray (int paramArr.[0]) (matchContinuosPeakID paramArr.[1]) (matchRealDataId paramArr.[2]) paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8]
                                             |> fun x -> (*printfn "Start creating string coded peaks"*)
                                                         x, createStringCodedPeaks x
                                             ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlie
                                             |> fun (gausInfo,codedStr) ->  (*printfn "Start adding noise"*)
                                                                            (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians
                         )
            |> Array.mapi (fun i (noisyArr,codedStr,emgaussians) -> i,(noisyArr |> Array.map fst), (noisyArr|> Array.map snd), codedStr, emgaussians
                          )
            |> Array.map (fun (id,xPos,yNoisy,codedStr,emgaussians) -> createLabeledSimulatedXic id xPos yNoisy codedStr emgaussians)
        //here will be imaging part
            |> fun x -> x.[0]
            |> fun (gausInfo) -> Array.zip3 gausInfo.CodedStringArray gausInfo.XPosition gausInfo.NoisyXicIntensity 
            |> fun arr ->   Array.filter (fun (x,y,z) -> x = "P") arr, Array.filter (fun (x,y,z) -> (String.contains "A" x )= true) arr,
                            Array.filter (fun (x,y,z) -> (String.contains "PP" x )= true) arr, Array.filter (fun (x,y,z) -> (String.contains "LE" x ) = true || (String.contains "RE" x ) = true ) arr,
                            Array.filter (fun (x,y,z) -> x = "X" ) arr, 
                            Array.filter (fun (x,y,z) -> x <> "P" && x <> "X" && (String.contains "PP" x ) = false && (String.contains "A" x )= false && (String.contains "LE" x ) = false && (String.contains "RE" x ) = false) arr
            |> fun (p,a,pp,lere,x,rest) ->   [
                                                (Array.unzip3 p |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Part Of Peak", Labels = str, Opacity = 0.8) ) )
                                                (Array.unzip3 a |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Apex", Labels = str) ) )
                                                (Array.unzip3 pp |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Overlap", Labels = str) ) )
                                                (Array.unzip3 lere |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Left End/Right End", Labels = str) ) )
                                                (Array.unzip3 x |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Not Part Of Peak", Labels = str, Opacity = 0.8) ) )
                                                (Array.unzip3 rest |> (fun (str,x,y) -> Chart.Point (x,y,Name = "Rest", Labels = str, Opacity = 0.5) ) )
    
                                            ]
            |> Chart.Combine
            |> Chart.withSize (1000.,600.)
            |> Chart.Show
        ////here end imaging part
            //|> Array.map (fun (id,xPos,yNoisy,codedStr,emgaussians) -> createCsvSimulatedXic id xPos yNoisy codedStr emgaussians)
            //|> Seq.ofArray
            //|> Seq.toCSV "," true
            //|> fun x -> if paramArr.Length = 17 then Seq.append [sprintf "# VersionNo. 1
            //                             # NumberOfPeaks = %A; Continuous Peaks = %A; EMGPropability = %A; MeanFrom-MeanTo = %A - %A; IntensityFrom-IntensityTO = %A - %A; IntensityNoiseStdv = %A;
            //                             # SigmaMean = %A; SigmaStdv = %A; TauMean = = %A; TauStdv = %A; Windowsize = %A; WhiteNoiseMean = %A; WhiteNoiseStdv = %A; Minimal distance between two peaks = %A sigma; minimal intensity difference at apex = %A" (int paramArr.[0]) paramArr.[1] paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8] paramArr.[9] paramArr.[10] paramArr.[11] paramArr.[12] paramArr.[13] paramArr.[14] paramArr.[15] paramArr.[16]
            //                                                    ] x
            //            elif paramArr.Length = 9 then Seq.append [sprintf "# VersionNo. 1
            //                              # NumberOfPeaks = %A; Continuous Peaks = %A; Origin RealDataSet for Parameters = %A; IntensityNoiseStdv = %A;
            //                              # Windowsize = %A; WhiteNoiseMean = %A; WhiteNoiseStdv = %A; Minimal distance between two peaks = %A sigma; minimal intensity difference at apex = %A" (int paramArr.[0]) paramArr.[1] paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8]
            //                                                     ] x
            //            else failwith "ParamArr must have length 8 or 16!"
            /////takes most of the time
            //|> Seq.write (filePath + (sprintf @"%A-%A-%A" System.DateTime.UtcNow.Year System.DateTime.UtcNow.Month System.DateTime.UtcNow.Day) + @"_SimulatedXics" + (String.replace ":" "-" (sprintf @"_%A" System.DateTime.UtcNow.TimeOfDay)) + @".csv" )
       
        createCodedGausWithNoises
    
    