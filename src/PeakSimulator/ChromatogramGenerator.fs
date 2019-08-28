namespace PeakSimulator


open FSharpAux
open FSharp.Stats
open PeakSimulator
open RealDataValues
open FSharp.Plotly


module ChromatogramFunctions =

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

    type RealDataInfo = {
        NOfPeaks : int
        RealDataId : RealData
        ContinuousPeaks : bool
        IntensityNoiseStdv : float
        WindowSize : float
        WhiteNoiseMean : float
        WhiteNoiseStdv : float
        MinDistancePeaks : float
        MinDifferenceIntensity : float
    }
    
    //nOfChromatograms:int,nOfPeaks:int,
    //?ContinuousPeaks:bool, ?EmgPropability:float, ?MeanFrom:float, ?MeanTo:float, ?IntensityFrom, ?IntensityTo, ?IntensityNoiseStdv ,
    //?SigmaMean, ?SigmaStdv, ?TauMean, ?TauStdv, ?WindowSize, ?WhiteNoiseMean, ?WhiteNoiseStdv, ?MinDistancePeaks, ?MinDifferenceIntensity)
    type RndDataInfo = {
        NOfPeaks : int
        ContinuousPeaks : bool
        EmgPropability : float
        MeanFrom : float
        MeanTo : float
        IntensityFrom : float
        IntensityTo : float
        IntensityNoiseStdv : float
        SigmaMean : float
        SigmaStdv : float
        TauMean : float
        TauStdv : float
        WindowSize : float
        WhiteNoiseMean : float
        WhiteNoiseStdv : float
        MinDistancePeaks : float
        MinDifferenceIntensity : float
    }

    type InputInfo =
    | RealData of RealDataInfo
    | RndData of RndDataInfo

    type PeakArrayInformation = {
        Chromatogram    : (float *float) []
        SymbolArray     : string []
        EMGaussians     : EMGaussian [] 
        InputParameters : InputInfo
    }

    let private createPeakArrayInformation chromatogram symbols emgaussians inputInfo = {
        Chromatogram    = chromatogram
        SymbolArray     = symbols
        EMGaussians     = emgaussians
        InputParameters = inputInfo
    }

    ///This function returns simulated peak array save in a .csv file.
    ///The function can create peaks based on random variables or on real data distributions. Depending on the chosen option the function takes paramArrays of different length.
    ///REALDATA: n = number of peak distributions (minimum 2); fromPremade = true; paramArr = NumberOfPeaks, continuousPeaks, RealData-ID (1.), IntensityNoiseStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity; filePate = @"C:\path\".
    ///RANDOMDATA: n = number of peak distributions (minimum 2); fromPremade = false; paramArr = NumberOfPeaks, continuousPeaks, emgPropability, meanFrom, meanTo, intensityFrom, intensityTo, 
    ///intensityNoiseStdv, sigmaMean, sigmaStdv, tauMean, tauStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity 
    //let chromatogramToCSV n (fromPremade:bool) (paramArr:float []) (filePath: string)=
    //    match fromPremade with
    //    | false -> if paramArr.Length <> 17 then failwith "paramArr must have either 9 parameters for PeakSimulation from RealData distributions or 17 for random PeakSimulation!" 
    //    | true -> if paramArr.Length <> 9 then failwith "paramArr must have either 9 parameters for PeakSimulation from RealData distributions or 17 for random PeakSimulation!" 
    //    if filePath.EndsWith @"\" = false then failwith @"filepath must end with \ (backslash)"
    //    //if n = 1 then failwith "chromatogramToCSV function must have at least n = 2"
    //    let matchRealDataId (value:float) =
    //        match value with
    //        | 1. -> NumberOne
    //        | _ -> failwith "Unknown RealData id, please use another paramater for realData (use: 1.)"
    //    let matchContinuosPeakID (value:float) =
    //        match value with
    //        | 1. -> true
    //        | 0. -> false
    //        | _ -> failwith "Value to determine continuous peak must be either 0 for non-continuous or 1 for continuous peaks"
    
    //    let addGausNoise (codedGaus: SimulatedXic) (gausArr:(float*float) []) =
    //        let yNoise =
    //            codedGaus.NoiseArray
    //            |> Array.unzip
    //            |> fun (x,y) -> y
    //        gausArr 
    //        |> Array.map2 (fun yNoise (x,yInt) -> x, yInt + yNoise) yNoise
    
    
    //    let addWhiteNoise' (codedGaus: SimulatedXic) (gausArr: (float*float)[]) =
    //        codedGaus.WhiteNoiseArray
    //        |> Array.map2 (fun (gX,gY) (wX,wY) -> gX,gY+wY) gausArr
    
    //    let createCodedGausWithNoises = 
    //        let doNTimes =
    //            [|0 .. n|]
    //        doNTimes 
    //        |> Array.map (fun _ -> match fromPremade with
    //                               | false -> createRndPeakArray (int paramArr.[0]) (matchContinuosPeakID paramArr.[1]) paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8] paramArr.[9] paramArr.[10] paramArr.[11] paramArr.[12] paramArr.[13] paramArr.[14] paramArr.[15] paramArr.[16]
    //                                          |> fun x -> (*printfn "Start creating string coded peaks"*)
    //                                                      x, createStringCodedPeaks x
    //                                          ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlier.
    //                                          |> fun (gausInfo,codedStr) -> (*printfn "Start adding noise"*)
    //                                                                        (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians 
    //                               | true -> createPeakArray (int paramArr.[0]) (matchContinuosPeakID paramArr.[1]) (matchRealDataId paramArr.[2]) paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8]
    //                                         |> fun x -> (*printfn "Start creating string coded peaks"*)
    //                                                     x, createStringCodedPeaks x
    //                                         ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlie
    //                                         |> fun (gausInfo,codedStr) ->  (*printfn "Start adding noise"*)
    //                                                                        (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians
    //                     )
    //        |> Array.mapi (fun i (noisyArr,codedStr,emgaussians) -> i,(noisyArr |> Array.map fst), (noisyArr|> Array.map snd), codedStr, emgaussians
    //                      )
    //        |> Array.map (fun (id,xPos,yNoisy,codedStr,emgaussians) -> createLabeledSimulatedXic id xPos yNoisy codedStr emgaussians)
    //        |> Seq.ofArray
    //        |> Seq.toCSV "," true
    //        |> fun x -> if paramArr.Length = 17 then Seq.append [sprintf "# VersionNo. 1
    //                                     # NumberOfPeaks = %A; Continuous Peaks = %A; EMGPropability = %A; MeanFrom-MeanTo = %A - %A; IntensityFrom-IntensityTO = %A - %A; IntensityNoiseStdv = %A;
    //                                     # SigmaMean = %A; SigmaStdv = %A; TauMean = = %A; TauStdv = %A; Windowsize = %A; WhiteNoiseMean = %A; WhiteNoiseStdv = %A; Minimal distance between two peaks = %A sigma; minimal intensity difference at apex = %A" (int paramArr.[0]) paramArr.[1] paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8] paramArr.[9] paramArr.[10] paramArr.[11] paramArr.[12] paramArr.[13] paramArr.[14] paramArr.[15] paramArr.[16]
    //                                                            ] x
    //                    elif paramArr.Length = 9 then Seq.append [sprintf "# VersionNo. 1
    //                                      # NumberOfPeaks = %A; Continuous Peaks = %A; Origin RealDataSet for Parameters = %A; IntensityNoiseStdv = %A;
    //                                      # Windowsize = %A; WhiteNoiseMean = %A; WhiteNoiseStdv = %A; Minimal distance between two peaks = %A sigma; minimal intensity difference at apex = %A" (int paramArr.[0]) paramArr.[1] paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8]
    //                                                             ] x
    //                    else failwith "ParamArr must have length 8 or 16!"
    //        ///takes most of the time
    //        |> Seq.write (filePath + (sprintf @"%A-%A-%A" System.DateTime.UtcNow.Year System.DateTime.UtcNow.Month System.DateTime.UtcNow.Day) + @"_SimulatedXics" + (String.replace ":" "-" (sprintf @"_%A" System.DateTime.UtcNow.TimeOfDay)) + @".csv" )
       
    //    createCodedGausWithNoises
    

    ///This function returns simulated peak array displays as point chart.
    ///The function can create peaks based on random variables or on real data distributions. Depending on the chosen option the function takes paramArrays of different length.
    ///REALDATA: n = number of peak distributions (minimum 2); fromPremade = true; paramArr = NumberOfPeaks, continuousPeaks, RealData-ID (1.), IntensityNoiseStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity; filePate = @"C:\path\".
    ///RANDOMDATA: n = number of peak distributions (minimum 2); fromPremade = false; paramArr = NumberOfPeaks, continuousPeaks, emgPropability, meanFrom, meanTo, intensityFrom, intensityTo, 
    ///intensityNoiseStdv, sigmaMean, sigmaStdv, tauMean, tauStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity 
    //let chromatogramToImg n (fromPremade:bool) (paramArr:float []) =
    //    match fromPremade with
    //    | false -> if paramArr.Length <> 17 then failwith "paramArr must have either 9 parameters for PeakSimulation from RealData distributions or 17 for random PeakSimulation!" 
    //    | true -> if paramArr.Length <> 9 then failwith "paramArr must have either 9 parameters for PeakSimulation from RealData distributions or 17 for random PeakSimulation!" 
    //    //if n = 1 then failwith "chromatogramToImg function must have at least n = 2"
    //    let matchRealDataId (value:float) =
    //        match value with
    //        | 1. -> NumberOne
    //        | _ -> failwith "Unknown RealData id, please use another paramater for realData (use: 1.)"
    //    let matchContinuosPeakID (value:float) =
    //        match value with
    //        | 1. -> true
    //        | 0. -> false
    //        | _ -> failwith "Value to determine continuous peak must be either 0 for non-continuous or 1 for continuous peaks"
    
    //    let addGausNoise (codedGaus: SimulatedXic) (gausArr:(float*float) []) =
    //        let yNoise =
    //            codedGaus.NoiseArray
    //            |> Array.unzip
    //            |> fun (x,y) -> y
    //        gausArr 
    //        |> Array.map2 (fun yNoise (x,yInt) -> x, yInt + yNoise) yNoise
    
    
    //    let addWhiteNoise' (codedGaus: SimulatedXic) (gausArr: (float*float)[]) =
    //        codedGaus.WhiteNoiseArray
    //        |> Array.map2 (fun (gX,gY) (wX,wY) -> gX,gY+wY) gausArr
    
    //    let createCodedGausWithNoises = 
    //        let doNTimes =
    //            [|0 .. n|]
    //        doNTimes 
    //        |> Array.map (fun _ -> match fromPremade with
    //                               | false -> createRndPeakArray (int paramArr.[0]) (matchContinuosPeakID paramArr.[1]) paramArr.[2] paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8] paramArr.[9] paramArr.[10] paramArr.[11] paramArr.[12] paramArr.[13] paramArr.[14] paramArr.[15] paramArr.[16]
    //                                          |> fun x -> (*printfn "Start creating string coded peaks"*)
    //                                                      x, createStringCodedPeaks x
    //                                          ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlier.
    //                                          |> fun (gausInfo,codedStr) -> (*printfn "Start adding noise"*)
    //                                                                        (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians 
    //                               | true -> createPeakArray (int paramArr.[0]) (matchContinuosPeakID paramArr.[1]) (matchRealDataId paramArr.[2]) paramArr.[3] paramArr.[4] paramArr.[5] paramArr.[6] paramArr.[7] paramArr.[8]
    //                                         |> fun x -> (*printfn "Start creating string coded peaks"*)
    //                                                     x, createStringCodedPeaks x
    //                                         ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlie
    //                                         |> fun (gausInfo,codedStr) ->  (*printfn "Start adding noise"*)
    //                                                                        (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians
    //                     )
    //    createCodedGausWithNoises

    ///RANDOMDATA: n = number of peak distributions (minimum 2); fromPremade = false; 
    /// paramArr = NumberOfPeaks, continuousPeaks, emgPropability, meanFrom, meanTo, intensityFrom, intensityTo, 
    /// intensityNoiseStdv, sigmaMean, sigmaStdv, tauMean, tauStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity 
    let peakArrOfRndData nOfChromatograms nOfPeaks (continuousPeaks:bool) emgPropability meanFrom meanTo intensityFrom intensityTo intensityNoiseStdv sigmaMean sigmaStdv tauMean tauStdv windowSize whiteNoiseMean whiteNoiseStdv minDistancePeaks minDifferenceIntensity =
    
        let inputData =
            RndData {
                NOfPeaks            = nOfPeaks
                ContinuousPeaks     = continuousPeaks
                EmgPropability      = emgPropability
                MeanFrom            = meanFrom
                MeanTo              = meanTo
                IntensityFrom       = intensityFrom
                IntensityTo         = intensityTo
                IntensityNoiseStdv  = intensityNoiseStdv
                SigmaMean           = sigmaMean
                SigmaStdv           = sigmaStdv
                TauMean             = tauMean
                TauStdv             = tauStdv
                WindowSize          = windowSize
                WhiteNoiseMean      = whiteNoiseMean
                WhiteNoiseStdv      = whiteNoiseStdv
                MinDistancePeaks    = minDistancePeaks
                MinDifferenceIntensity = minDifferenceIntensity
            }

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
                [|0 .. nOfChromatograms|]
            doNTimes 
            |> Array.map (fun _ -> createRndPeakArray nOfPeaks continuousPeaks emgPropability meanFrom meanTo intensityFrom intensityTo intensityNoiseStdv sigmaMean sigmaStdv tauMean tauStdv windowSize whiteNoiseMean whiteNoiseStdv minDistancePeaks minDifferenceIntensity
                                              |> fun x -> (*printfn "Start creating string coded peaks"*)
                                                          x, createStringCodedPeaks x
                                              ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlier.
                                              |> fun (gausInfo,codedStr) -> (*printfn "Start adding noise"*)
                                                                            (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians 
                                              |> fun (x,y,z) -> createPeakArrayInformation x y z inputData
            )
        createCodedGausWithNoises

    ///REALDATA: n = number of peak distributions (minimum 2); fromPremade = true; paramArr = NumberOfPeaks, 
    //continuousPeaks, RealData-ID (1.), 
    // IntensityNoiseStdv, windowSize, whiteNoiseMean, whiteNoiseStdv, minDistancePeaks, minDifferenceIntensity; filePate = @"C:\path\".
    let peakArrOfRealData nOfChromatograms nOfPeaks (realDataModel:RealData) (continuousPeaks:bool) intensityNoiseStdv windowSize whiteNoiseMean whiteNoiseStdv minDistancePeaks minDifferenceIntensity=

        let inputData =
            RealData {
                NOfPeaks            = nOfPeaks
                RealDataId          = realDataModel
                ContinuousPeaks     = continuousPeaks
                IntensityNoiseStdv  = intensityNoiseStdv
                WindowSize          = windowSize
                WhiteNoiseMean      = whiteNoiseMean
                WhiteNoiseStdv      = whiteNoiseStdv
                MinDistancePeaks    = minDistancePeaks
                MinDifferenceIntensity = minDifferenceIntensity
            }

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
                [|0 .. nOfChromatograms|]
            doNTimes 
            |> Array.map (fun _ -> createPeakArray nOfPeaks continuousPeaks realDataModel intensityNoiseStdv windowSize whiteNoiseMean whiteNoiseStdv minDistancePeaks minDifferenceIntensity
                                   |> fun x -> (*printfn "Start creating string coded peaks"*)
                                               x, createStringCodedPeaks x
                                   ///TODO: ask! In the next step lots of information is lost (gausInfo). if this is fine, then throw it away way earlie
                                   |> fun (gausInfo,codedStr) ->  (*printfn "Start adding noise"*)
                                                                  (addGausNoise gausInfo gausInfo.DistributionArray) |> addWhiteNoise' gausInfo, codedStr, gausInfo.EMGaussians
                                   |> fun (x,y,z) -> createPeakArrayInformation x y z inputData
                         )
        createCodedGausWithNoises

    let imgOfPeakArr (peakArrArr:PeakArrayInformation []) =
        peakArrArr
        |> Array.mapi (fun i (peakArr) -> i,(peakArr.Chromatogram |> Array.map fst), (peakArr.Chromatogram |> Array.map snd), peakArr.SymbolArray, peakArr.EMGaussians
                      )
        |> Array.map (fun (id,xPos,yNoisy,codedStr,emgaussians) -> createLabeledSimulatedXic id xPos yNoisy codedStr emgaussians)
        |> Array.map (fun (gausInfo) -> Array.zip3 gausInfo.CodedStringArray gausInfo.XPosition gausInfo.NoisyXicIntensity 
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
        )

    let fileOfPeakArr filePath (peakArrArr:PeakArrayInformation []) =
        peakArrArr
        |> Array.mapi (fun i (peakArr) -> i,(peakArr.Chromatogram |> Array.map fst), (peakArr.Chromatogram |> Array.map snd), peakArr.SymbolArray, peakArr.EMGaussians
                      )
        |> Array.map (fun (id,xPos,yNoisy,codedStr,emgaussians) -> createLabeledSimulatedXic id xPos yNoisy codedStr emgaussians)
        |> id
        |> Seq.ofArray
        |> Seq.toCSV "\t" true
        |> fun x -> match peakArrArr.[0].InputParameters with
                    | RndData rndDataInfo -> 
                        Seq.append [sprintf "# VersionNo. 1
                                     # NumberOfPeaks = %A # Continuous Peaks = %A # EMGPropability = %A # MeanFrom-MeanTo = %A - %A # IntensityFrom-IntensityTO = %A - %A # IntensityNoiseStdv = %A;
                                     # SigmaMean = %A # SigmaStdv = %A # TauMean = = %A # TauStdv = %A # Windowsize = %A # WhiteNoiseMean = %A # WhiteNoiseStdv = %A # Minimal distance between two peaks = %A sigma # minimal intensity difference at apex = %A" 
                                     rndDataInfo.NOfPeaks rndDataInfo.ContinuousPeaks rndDataInfo.EmgPropability rndDataInfo.MeanFrom rndDataInfo.MeanTo 
                                     rndDataInfo.IntensityFrom rndDataInfo.IntensityTo rndDataInfo.IntensityNoiseStdv rndDataInfo.SigmaMean rndDataInfo.SigmaStdv 
                                     rndDataInfo.TauMean rndDataInfo.TauStdv rndDataInfo.WindowSize rndDataInfo.WhiteNoiseMean rndDataInfo.WhiteNoiseStdv rndDataInfo.MinDistancePeaks rndDataInfo.MinDifferenceIntensity
                        ] x
                    | RealData realDatainfo ->
                        Seq.append [sprintf "# VersionNo. 1
                                      # NumberOfPeaks = %A # Continuous Peaks = %A # Origin RealDataSet for Parameters = %A # IntensityNoiseStdv = %A
                                      # Windowsize = %A # WhiteNoiseMean = %A # WhiteNoiseStdv = %A # Minimal distance between two peaks = %A sigma # minimal intensity difference at apex = %A" 
                                      realDatainfo.NOfPeaks realDatainfo.ContinuousPeaks realDatainfo.RealDataId realDatainfo.IntensityNoiseStdv realDatainfo.WindowSize 
                                      realDatainfo.WhiteNoiseMean realDatainfo.WhiteNoiseStdv realDatainfo.MinDistancePeaks realDatainfo.MinDifferenceIntensity
                        ] x
        ///takes most of the time
        |> Seq.write (filePath + (sprintf @"%A-%A-%A" System.DateTime.UtcNow.Year System.DateTime.UtcNow.Month System.DateTime.UtcNow.Day) + @"_SimulatedXics" + (String.replace ":" "-" (sprintf @"_%A" System.DateTime.UtcNow.TimeOfDay)) + @".txt" )
       

open ChromatogramFunctions
open SimulatePeak.AuxFunctions.AuxTypes

type Chromatograms =
    
    ///This function returns n1 chromatograms each with n2 peaks based on real data distributions. 
    static member createPeakInformation(nOfChromatograms:int,nOfPeaks:int,realDataId:RealData,?ContinuousPeaks:bool,?IntensityNoiseStdv:float,?WindowSize:float,?WhiteNoiseMean,?WhiteNoiseStdv,?MinDistancePeaks,?MinDifferenceIntensity) =

        //define standard values
        let baseContinuesPeaks =
            if ContinuousPeaks.IsNone then true else ContinuousPeaks.Value

        let baseIntensityNoiseStdv =
            if IntensityNoiseStdv.IsNone then 0.3 else IntensityNoiseStdv.Value

        let baseWindowSize =
            if WindowSize.IsNone then 0.01 else WindowSize.Value

        let baseWhiteNoiseMean =
            if WhiteNoiseMean.IsNone then 1000. else WhiteNoiseMean.Value

        let baseWhiteNoiseStdv =
            if WhiteNoiseStdv.IsNone then 4000. else WhiteNoiseStdv.Value
            
        let baseMindDistancePeaks =
            if MinDistancePeaks.IsNone then 2. else MinDistancePeaks.Value

        let baseMinDifferenceIntensity =
            if MinDifferenceIntensity.IsNone then 2. else MinDifferenceIntensity.Value

        peakArrOfRealData nOfChromatograms nOfPeaks realDataId baseContinuesPeaks baseIntensityNoiseStdv baseWindowSize baseWhiteNoiseMean baseWhiteNoiseStdv baseMindDistancePeaks baseMinDifferenceIntensity


    ///This function returns n1 chromatograms each with n2 peaks based the given variables.
    static member createPeakInformation(nOfChromatograms:int,nOfPeaks:int,
                                        ?ContinuousPeaks:bool, ?EmgPropability:float, ?MeanFrom:float, ?MeanTo:float, ?IntensityFrom, ?IntensityTo, ?IntensityNoiseStdv ,
                                        ?SigmaMean, ?SigmaStdv, ?TauMean, ?TauStdv, ?WindowSize, ?WhiteNoiseMean, ?WhiteNoiseStdv, ?MinDistancePeaks, ?MinDifferenceIntensity) =

        //define standard values
        let baseContinuesPeaks =
            if ContinuousPeaks.IsNone then true else ContinuousPeaks.Value

        let baseEmgPropability =
            if EmgPropability.IsNone then 0.55 else EmgPropability.Value

        let baseMeanFrom =
            if MeanFrom.IsNone then 0. else MeanFrom.Value

        let baseMeanTo =
            if MeanTo.IsNone then 80. else MeanTo.Value

        let baseIntensityFrom =
            if IntensityFrom.IsNone then 10000. else IntensityFrom.Value

        let baseIntensityTo =
            if IntensityTo.IsNone then 999999. else IntensityTo.Value

        let baseIntensityNoiseStdv =
            if IntensityNoiseStdv.IsNone then 0.3 else IntensityNoiseStdv.Value

        let baseSigmaMean =
            if SigmaMean.IsNone then 1. else SigmaMean.Value

        let baseSigmaStdv =
            if SigmaStdv.IsNone then 0.5 else SigmaStdv.Value

        let baseTauMean =
            if TauMean.IsNone then 0.8 else TauMean.Value

        let baseTauStdv =
            if TauStdv.IsNone then 0.2 else TauStdv.Value

        let baseWindowSize =
            if WindowSize.IsNone then 0.01 else WindowSize.Value

        let baseWhiteNoiseMean =
            if WhiteNoiseMean.IsNone then 1000. else WhiteNoiseMean.Value

        let baseWhiteNoiseStdv =
            if WhiteNoiseStdv.IsNone then 4000. else WhiteNoiseStdv.Value
            
        let baseMindDistancePeaks =
            if MinDistancePeaks.IsNone then 2. else MinDistancePeaks.Value

        let baseMinDifferenceIntensity =
            if MinDifferenceIntensity.IsNone then 2. else MinDifferenceIntensity.Value

        peakArrOfRndData nOfChromatograms nOfPeaks baseContinuesPeaks baseEmgPropability baseMeanFrom baseMeanTo baseIntensityFrom baseIntensityTo baseIntensityNoiseStdv
                         baseSigmaMean baseSigmaStdv baseTauMean baseTauStdv baseWindowSize baseWhiteNoiseMean baseWhiteNoiseStdv baseMindDistancePeaks baseMinDifferenceIntensity
        
    /// This function returns a FSharp.Plotly Point Chart of a PeakArrayInformation [], created by the createPeakInformation - function.
    static member peakArrToImg(peakinfoArr) =
        peakinfoArr
        |> imgOfPeakArr

    /// This function saves the information of a PeakArrayInformation [], created by the createPeakInformation - function, as a .txt file.
    static member peakArrToTxt(filePath,peakinfoArr:PeakArrayInformation []) =
        peakinfoArr
        |> fileOfPeakArr filePath
