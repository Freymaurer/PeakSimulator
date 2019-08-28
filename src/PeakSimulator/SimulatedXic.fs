namespace PeakSimulator


module SimulatePeak =

    module AuxFunctions =
    
        open FSharp.Stats
    
        module AuxTypes =
    
            ///
            type Extrema =
            ///annoying errors where a lot of None options where "meant to be Extreme", therefore change name to "NoExtrema"
                | NoExtrema 
                | Positive 
                | Negative 
        
            ///
            type PeakFeature = {
                Index :int
                XVal  :float
                YVal  :float
                }
        
            ///
            type IdentifiedPeak = {
                Apex                 : PeakFeature 
                LeftLiftOff          : PeakFeature option 
                LeftEnd              : PeakFeature
                RightLiftOff         : PeakFeature option 
                RightEnd             : PeakFeature 
                XData                : float []
                YData                : float []
                }
    
            ///
            type EMGData = {
                Runtime : float
                NumberOfPeaks: float
                IntensityDistribution : Map<float,int>
                IntensityBandwidth : float
                SigmaDistribution : Map<float,int>
                SigmaBandwidth  : float 
                TauDistribution: Map<float,int>
                TauBandwidth : float 
                MeanDistribution : Map<float,int>
                MeanBandwidth : float
                }
            
            ///
            type GausData = {
                Runtime : float
                NumberOfPeaks: float
                IntensityDistribution : Map<float,int>
                IntensityBandwidth : float
                SigmaDistribution : Map<float,int>
                SigmaBandwidth  : float
                MeanDistribution : Map<float,int>
                MeanBandwidth : float
                }
    
            ///
            type RealData = {
                GausDataInfo : GausData
                EMGDataInfo : EMGData
                }
    
            ////types////
            type EMGaussian = {
                Mean        : float
                Sigma       : float
                Intensity   : float
                Tau         : float
                Mode        : float
                }
            
            ///
            type SimulatedTestXic = {
                EMGaussians                  : EMGaussian []
                // TODO:Better Name
                DistributionArray            : (float*float) []
                NoiseArray                   : (float*float) []
                }
    
            ///
            let createSimulatedTestXic gausDistributions gaussians gausNoiseArr = {
                    EMGaussians = gaussians
                    DistributionArray = gausDistributions
                    NoiseArray = gausNoiseArr
                }
            
            ///
            type SimulatedXic = {
                EMGaussians                   : EMGaussian []
                DistributionArray             : (float*float) []
                NoiseArray                    : (float*float) [] 
                WhiteNoiseArray               : (float*float) []
                PeakInformartions             : IdentifiedPeak []
                }
            
            ///        
            type Tag<'t,'v> = {
                Meta : 't
                Data : 'v
                }
        
            ///
            let createPeakFeature index xVal yVal = {
                Index = index
                XVal  = xVal 
                YVal  = yVal 
                }    
        
            ///
            let createIdentifiedPeak apex leftLiftOff leftEnd rightLiftOff rightEnd xData yData = {
                Apex                = apex                
                LeftLiftOff         = leftLiftOff         
                LeftEnd             = leftEnd             
                RightLiftOff        = rightLiftOff        
                RightEnd            = rightEnd            
                XData               = xData               
                YData               = yData               
                }    
    
            ///
            let createEMGData runtime numberOfPeaks intensityDistribution intBandwidth sigmaDis sigmaBandwidth tauDis tauBandwidth meanDis meanBandwidth = {      
                Runtime                     = runtime
                NumberOfPeaks               = numberOfPeaks
                IntensityDistribution       = intensityDistribution
                IntensityBandwidth          = intBandwidth
                SigmaDistribution           = sigmaDis
                SigmaBandwidth              = sigmaBandwidth 
                TauDistribution             = tauDis
                TauBandwidth                = tauBandwidth
                MeanDistribution            = meanDis
                MeanBandwidth               = meanBandwidth
                }
    
            ///
            let createGausData runtime numberOfPeaks intensityDistribution intensityBandwidth sigmaDis sigmaBandwidth meanDis meanBandwidth = {
                Runtime                     = runtime
                NumberOfPeaks               = numberOfPeaks
                IntensityDistribution       = intensityDistribution
                IntensityBandwidth          = intensityBandwidth
                SigmaDistribution           = sigmaDis
                SigmaBandwidth              = sigmaBandwidth
                MeanDistribution            = meanDis
                MeanBandwidth               = meanBandwidth
                }
    
            ///
            let createEMGaussian mean sigma intensity tau mode= {
                Mean = mean
                Sigma = sigma
                Intensity = intensity
                Tau = tau
                Mode = mode
                }
            
            ///
            let createSimulatedXic whiteNoise gausArr gausNoise gaussiansArr peakInfos = {
                    EMGaussians = gaussiansArr
                    DistributionArray = gausArr
                    NoiseArray = gausNoise
                    WhiteNoiseArray = whiteNoise  
                    PeakInformartions = peakInfos
                }
    
        module RealData = 
    
            open System.IO
            open FSharpAux
            open AuxTypes
    
            let getRelativeFilePath str =
                let sourceDir = __SOURCE_DIRECTORY__
                let parentDir = Directory.GetParent(sourceDir)
                let getGrandParentDir = Directory.GetParent(parentDir.FullName)
                getGrandParentDir.FullName + @"\" + str
    
            let readDistributionInfo (path:string) = 
                
                let readLines (filePath:string) = seq {
                    use sr = new StreamReader (filePath)
                    while not sr.EndOfStream do
                        yield sr.ReadLine ()
                    }
                let translateStringToDistributionMap (strArr: seq<string>)=
                    strArr
                    |> Seq.toArray
                    |> Array.map (fun x -> String.split ',' x
                                            |> Array.map (fun x -> float x)
                                 )
                    |> Array.map (fun x -> x.[0], int x.[1])
                    |> Map.ofArray
                    |> fun x -> x
                readLines path
                |> translateStringToDistributionMap
            
            let getStandardIntensityDistribution (path:string) = 
                
                let readLines (filePath:string) = seq {
                    use sr = new StreamReader (filePath)
                    while not sr.EndOfStream do
                        yield sr.ReadLine ()
                    }
                let translateString (strArr: seq<string>)=
                    strArr
                    |> Seq.toArray
                    |> Array.map (fun x -> String.split ',' x
                                            |> Array.map (fun x -> float x)
                                 )
                    |> Array.map (fun x -> x.[0], x.[1])
                readLines path
                |> translateString
    
            ///always read in? how to delay until needed
            let NumberOne = {
                GausDataInfo = createGausData 250. 
                                              2201996.
                                              (readDistributionInfo (getRelativeFilePath @"material\ONEIntensityGausBinMap.txt"))
                                              6009.892902 // IntensityBandwidth
                                              (readDistributionInfo (getRelativeFilePath @"material\ONESigmaGausBinMap.txt"))
                                              0.05955576528 // SigmaBandwidth
                                              (readDistributionInfo (getRelativeFilePath @"material\ONEMeanGausBinMap.txt"))
                                              67.591528 // MeanBandwidth
                EMGDataInfo = createEMGData 250. 
                                            4279175. 
                                            (readDistributionInfo (getRelativeFilePath @"material\ONEIntensityEMGBinMap.txt")) 
                                            12372.99079 // emgIntensityBandwidth
                                            (readDistributionInfo (getRelativeFilePath @"material\ONESigmaEMGBinMap.txt")) 
                                            0.02911801746 // emgSigmaBandwidth
                                            (readDistributionInfo (getRelativeFilePath @"material\ONETauEMGBinMap.txt"))
                                            0.04925507138 // emgTauBandwidth
                                            (readDistributionInfo (getRelativeFilePath @"material\ONEMeanEMGBinMap.txt"))
                                            53.03298668 // emgMeanBandwidth
                }
    
        module AuxDistributions =
    
            open FSharpAux
            open FSharp.Stats.SpecialFunctions
            open FSharp.Stats.Fitting.NonLinearRegression.Table
    
            let emg meanX sigma amp tau xPos =
                emgModel.GetFunctionValue ([|amp;meanX;sigma;tau|]|> Vector.ofArray) xPos
                |> fun x -> if isNan x = true then 0. else x
    
            let reversErfcxX approxNumx num = 
                let x0 = 1./(num * sqrt(pi))
                let rec approximate var count =
                    if count <= approxNumx 
                        then var - (Errorfunction.erfcx(var) - num)/((2.*var*Errorfunction.erfcx(var)) - (2./sqrt(pi)))
                             |> fun x -> approximate x (count+1)
                        else var
                approximate x0 0
    
            ///This function returns the x-axis position of the exponentially modified gaus mode. 
            ///works accuratly for tau <= (2.5*sigma). The beginning inaccuracy at tau > (2.5 * sigma) increases as both tau and sigma increase. This function should not be used for tau > (3.*sigma).
            let modeOfEMG mean sigma tau = 
                ///Paper https://onlinelibrary.wiley.com/doi/full/10.1002/cem.1343
                let sgnTau = match tau with
                                | positive when tau > 0. -> 1.
                                | negative when tau < 0. -> -1.
                                | zero when tau = 0. -> 0.
                                | _ -> failwith "no real number as input"
                let erfcx = ((abs (tau))/sigma) * sqrt(2./pi)
                let revErfcs = reversErfcxX 5 erfcx
                mean - sgnTau * sqrt(2.) * sigma * revErfcs + ((sigma**2.)/tau)
    
    
            ////Start of creating random gaussian distribution//////////////////////////////////////////////////////
        
            //////////////////////////////////RandomGenerators/////////////////////////////////////////////////////////
            ///gives back random value between "meanFrom" and "meanTo" and 
            ///adds random value between 0. and 1., rounded to the place defined by digitsToRound.
            let genRandomMean meanFrom meanTo = 
                let target = FSharp.Stats.Random.rndgen
                let diff = meanTo - meanFrom
                (target.NextFloat() * diff) + meanFrom
    
            let genRandomIntensity distribution minInt maxInt = 
                    let target = FSharp.Stats.Random.rndgen
                    let diff = maxInt - minInt
                    target.NextFloat()
                    |> fun rnd -> Array.tryFind (fun (x,y) -> y >= rnd) distribution
                    |> fun x -> ((fst x.Value) * diff) + minInt
            
            let sampleOfGaussian mean stdv  =
                FSharp.Stats.Distributions.Continuous.Normal.Sample mean stdv
            
            let sampleArrOfGaussian n mean stdv =
                Array.init n ( fun _ -> sampleOfGaussian mean stdv )
    
            ///
            let filterEMG (baseArr:float[]) windowSize (findArr:float[]) =
                let filterRange = if (20.*windowSize) > 1. then 1. else (20.*windowSize)
                findArr 
                |> Array.map (fun findFloat -> baseArr  
                                               |> Array.mapi (fun i x -> x,i)
                                               |> Array.filter (fun (elem,i) -> elem <= (findFloat+filterRange) //TODO: Maybe have this flexible
                                                                                && elem >= (findFloat-filterRange)) //TODO: Maybe have this flexible
                                               |> Array.map (fun (arrFloat,i) -> abs (findFloat - arrFloat),i )
                                               |> Array.sort
                             )
                |> Array.filter (fun x -> (Array.isEmpty x) = false)
                |> fun x -> if (Array.isEmpty x) = true then failwith "Not a single peak was detected during" else x ///TODO: no failwith maybe try and loop
                |> Array.map (fun x -> snd (x.[0])) 
    
        module AuxPeakFinding =
            
            open AuxTypes
            open AuxDistributions
        
             ///not sure about <= and >=
            let private iUntili (predicate: int -> 'T -> bool) stepSize startIdx (arr: 'T []) =
                let rec loop  (arr: 'T []) currentIdx =
                    if currentIdx < 0 then None
                    elif currentIdx > arr.Length-1 then None
                    else                                              
                        match predicate currentIdx arr.[currentIdx] with 
                        | true -> Some currentIdx   
                        | _               -> loop arr (currentIdx+stepSize) 
                loop arr startIdx        
    
            ///not sure about <= and >=
            let private iUntil (predicate: 'T -> bool) stepSize startIdx (arr: 'T []) =
                let rec loop  (arr: 'T []) currentIdx =
                    if currentIdx < 0 then None
                    elif currentIdx > arr.Length-1 then None
                    else                                              
                        match predicate arr.[currentIdx] with 
                        | true -> Some currentIdx   
                        | _               -> loop arr (currentIdx+stepSize) 
                loop arr startIdx 
        
            /// Returns a collection of local Maxima and Minima. Attention: The algorithm is very sensitive to noise   
            let findPeakExtrema negYThreshold posYThreshold (xData:float[]) (smoothYData:float[]) =
                if xData.Length <= 5 then [||]
                else
                [|for i = 0 to xData.Length-1 do 
                    if i < 3 || i > xData.Length-4 then
                        yield {Meta=Extrema.NoExtrema; Data= xData.[i],smoothYData.[i]}
                                        
                    elif (smoothYData.[i] > posYThreshold && smoothYData.[i] > smoothYData.[i - 1] && smoothYData.[i] > smoothYData.[i + 1] 
                        && smoothYData.[i - 1] >= smoothYData.[i - 2] && smoothYData.[i + 1] >= smoothYData.[i + 2]) then
                        yield {Meta=Extrema.Positive; Data= xData.[i],smoothYData.[i]} //TODO: Typ is tin Peak.fs definiert, creatorFunktion verwenden
                        //TODO: try without >= after first change
                    //symmetric actual peak at 0.5 but can only see 0 and 1 will lead to no peak detection because x = x+1; therefore the following should resolve this problem
                    elif (smoothYData.[i] > posYThreshold 
                        && smoothYData.[i] > smoothYData.[i - 1] && smoothYData.[i] = smoothYData.[i + 1] 
                        && smoothYData.[i - 1] >= smoothYData.[i - 2] && smoothYData.[i + 1] > smoothYData.[i + 2]) && smoothYData.[i + 2] >= smoothYData.[i + 3] then
                        yield {Meta=Extrema.Positive; Data= xData.[i],smoothYData.[i]}
                    // Peak must be concave in the interval [i-2 .. i+2] and exheat a min hight (min_dh)
                    elif (smoothYData.[i] < negYThreshold && smoothYData.[i] < smoothYData.[i - 1] && smoothYData.[i] < smoothYData.[i + 1]  //smoothYData.[i] > yThreshold
                        && smoothYData.[i - 1] <= smoothYData.[i - 2] && smoothYData.[i + 1] <= smoothYData.[i + 2]) then
                        yield {Meta=Extrema.Negative; Data= xData.[i],smoothYData.[i]}
                    else
                        yield {Meta=Extrema.NoExtrema; Data= xData.[i],smoothYData.[i]}
                    |]    
       
        
            let savitzky_golay (window_size:int) (order:int) deriv rate (data:float[]) =
                ///             
                let correlate_valid (x:Vector<float>) (y:Vector<float>) =
                    if x.Length >= y.Length then 
                        vector [Vector.dot x y]
                    else
                        let n = x.Length
                        vector [ for i=1 to y.Length-n do
                                    yield Vector.dot x y.[i..i+n-1] ]
        
        
                if window_size % 2 <> 1 || window_size < 1 then
                    failwith "window_size size must be a positive odd number"
                if window_size < order + 2 then
                    failwith "window_size is too small for the polynomials order"
                //let order_range = [0..order]
                let half_window = (window_size - 1) / 2
                // precompute coefficients
                let b = Matrix.init (half_window*2 + 1) (order+1) (fun k coli -> float(k-half_window)**float(coli))   
        
                ///TODO: IndexOutOfRangeException!!!!! ERROR
                let m = (Algebra.LinearAlgebraManaged.pseudoInvers b).Row(deriv) * ((float(rate)**float(deriv)) * SpecialFunctions.Factorial.factorial(deriv))
                //pad the signal at the extremes with values taken from the signal itself
        
                let firstvals = 
                    let length = half_window + 1    
                    Array.init length (fun i -> 
                        data.[0] - (abs data.[length-i] - data.[0]))
        
                let lastvals = 
                    Array.init half_window (fun i -> 
                        data.[data.Length-1] - (abs data.[data.Length-(2+i)] - data.[data.Length-1]) ) 
               
                let y = 
                    Array.concat [firstvals; data; lastvals;] |> vector
        
                correlate_valid m.Transpose y
                 
            // Step 5: find rightLiftOff
            let closestLiftOffIdx stepSize labeledSndDevData peakIdx   = 
                iUntil (fun (x:Tag<Extrema,(float*float)>) -> x.Meta = Extrema.Negative) stepSize (peakIdx + stepSize)  labeledSndDevData 
        
            // Step 4I: find leftLiftOffIdx
            let closestLeftLiftOffIdx labeledSndDevData peakIdx =
                closestLiftOffIdx (-1) labeledSndDevData peakIdx
        
            // Step 5: find rightLiftOff
            let closestRightLiftOffIdx labeledSndDevData peakIdx = 
                closestLiftOffIdx (+1) labeledSndDevData peakIdx
         
            ///
            let elongateEMG (step:float) (emgaussian:EMGaussian) (whiteNoiseMean:float) =
                let yAtMode = emg emgaussian.Mean emgaussian.Sigma emgaussian.Intensity emgaussian.Tau emgaussian.Mode
                let rec loopElongation (xPos,intensity) =
                    if intensity < whiteNoiseMean || intensity < (yAtMode * 0.001) then (xPos,intensity)
                    else loopElongation ((xPos+step), (emg emgaussian.Mean emgaussian.Sigma emgaussian.Intensity emgaussian.Tau (xPos+step)))
                loopElongation (emgaussian.Mode, yAtMode)
            
            let mutable testVariable: (float*int*int) [] = [||]
            let mutable testVariable2: (float*float) [] = [||]
            let mutable testVariable3: EMGaussian [] = [||]
    
            ///
            let labelPeaksContinuous (xData: float[]) (apexi: PeakFeature []) (peakRanges: (float*float)[]) whiteNoiseMean (foundPeaks: EMGaussian [])=
                //printfn "foundPeaks.Length = %i" foundPeaks.Length
                if peakRanges.Length <> foundPeaks.Length then failwith "Error 01."
                if apexi.Length <> foundPeaks.Length then failwith "Error 02."
                let startArr = xData |> Array.mapi (fun i x -> x,i,-10)
                let rec loop (labeledArr:(float*int*int)[]) count =
                    let mutable countPeakStarted = false
                    let mutable nextCountPeakStarted = false
                    if count >= (peakRanges.Length-1)
                    then labeledArr
                    else labeledArr  ///active pattern matching
                         |> Array.map (fun (xValue,ind,lable) -> if (xValue > fst peakRanges.[count] || xValue > fst peakRanges.[count+1]) && (xValue < snd peakRanges.[count] || xValue < snd peakRanges.[count+1])
                                                                    || (nextCountPeakStarted = true && lable < (count+1) && lable <> -10)
                                                                 then let yOfEMG = (emg foundPeaks.[count].Mean foundPeaks.[count].Sigma foundPeaks.[count].Intensity foundPeaks.[count].Tau xValue)
                                                                      let yOfNextEMG = (emg foundPeaks.[count+1].Mean foundPeaks.[count+1].Sigma foundPeaks.[count+1].Intensity foundPeaks.[count+1].Tau xValue)
                                                                      if countPeakStarted = false 
                                                                      then if (lable >= count || lable = -10)
                                                                               && yOfEMG >= yOfNextEMG
                                                                               && yOfEMG >= (apexi.[count].YVal*0.001) && yOfEMG > whiteNoiseMean
                                                                           then countPeakStarted <- true
                                                                                xValue,ind,count
                                                                           elif (lable >= count || lable = -10)
                                                                                 && yOfEMG < yOfNextEMG
                                                                                 && yOfNextEMG >= (apexi.[count+1].YVal*0.001) && yOfNextEMG > whiteNoiseMean
                                                                           then xValue,ind,count
                                                                           else xValue,ind,lable
                                                                      else if nextCountPeakStarted = false 
                                                                           then if (lable >= count || lable = -10)
                                                                                    && yOfEMG >= yOfNextEMG
                                                                                    && yOfEMG >= (apexi.[count].YVal*0.001) && yOfEMG > whiteNoiseMean
                                                                                then xValue,ind,count
                                                                                elif (lable >= count || lable = -10)
                                                                                      && yOfEMG < yOfNextEMG
                                                                                      && yOfNextEMG >= (apexi.[count+1].YVal*0.001) && yOfNextEMG > whiteNoiseMean
                                                                                then nextCountPeakStarted <- true
                                                                                     xValue,ind,(count+1)
                                                                                else xValue,ind,lable
                                                                            else if (lable >= count || lable = -10)
                                                                                     && yOfEMG >= yOfNextEMG
                                                                                     && yOfEMG >= (apexi.[count].YVal*0.001) && yOfEMG > whiteNoiseMean
                                                                                 then xValue,ind,(count+1)
                                                                                 elif (lable >= count || lable = -10)
                                                                                       && yOfEMG < yOfNextEMG
                                                                                       && yOfNextEMG >= (apexi.[count+1].YVal*0.001) && yOfNextEMG > whiteNoiseMean
                                                                                 then xValue,ind,(count+1)
                                                                                 elif lable <> -10
                                                                                 then xValue,ind,(count+1)
                                                                                 else xValue,ind,lable
                                                                 else xValue,ind,lable
                                      )
                         |> fun x -> loop x (count+1)
                loop startArr 0
    
            ///
            let labelPeaks (xData: float[]) (apexi: PeakFeature []) (peakRanges: (float*float)[]) whiteNoiseMean (foundPeaks: EMGaussian [])=
                //printfn "foundPeaks.Length = %i" foundPeaks.Length
                if peakRanges.Length <> foundPeaks.Length then failwith "Error 05."
                if apexi.Length <> foundPeaks.Length then failwith "Error 06."
                let startArr = xData |> Array.mapi (fun i x -> x,i,-10,0.)
                let rec loop (labeledArr:(float*int*int*float)[]) count =
                    if count >= (peakRanges.Length-1)
                    then labeledArr
                    else labeledArr 
                         |> Array.map (fun (xValue,ind,lable,highScore) -> if xValue > fst peakRanges.[count] && xValue < snd peakRanges.[count+1]
                                                                           then let yOfEMG = (emg foundPeaks.[count].Mean foundPeaks.[count].Sigma foundPeaks.[count].Intensity foundPeaks.[count].Tau xValue)
                                                                                let yOfNextEMG = (emg foundPeaks.[count+1].Mean foundPeaks.[count+1].Sigma foundPeaks.[count+1].Intensity foundPeaks.[count+1].Tau xValue)
                                                                                if yOfEMG >= yOfNextEMG
                                                                                   && yOfEMG >= highScore
                                                                                   && yOfEMG >= (apexi.[count].YVal*0.001) && yOfEMG > 1000.
                                                                                then xValue,ind,count,yOfEMG
                                                                                elif yOfEMG < yOfNextEMG
                                                                                     && yOfNextEMG >= highScore
                                                                                     && yOfNextEMG >= (apexi.[count+1].YVal*0.001) && yOfNextEMG > 1000.
                                                                                then xValue,ind,(count+1), yOfNextEMG
                                                                                else xValue,ind,lable,highScore
                                                                           else xValue,ind,lable,highScore
                                      )
                         |> fun x -> loop x (count+1)
                loop startArr 0
                |> Array.map (fun (x,y,z,a) -> x,y,z)
    
    
            ///
            let characterizePeak (xData: float []) (yData: float []) (labeledSndDevData: Tag<Extrema,(float*float)> []) (peakIdx: int) = 
                let apex = createPeakFeature peakIdx xData.[peakIdx] yData.[peakIdx]
                let leftLiftOffIdx = closestLeftLiftOffIdx labeledSndDevData peakIdx  
                let leftLiftOff = 
                    match leftLiftOffIdx with 
                    | Option.Some i -> Option.Some (createPeakFeature i xData.[i] yData.[i])
                    | Option.None   -> Option.None
                let rightLiftOffIdx = closestRightLiftOffIdx labeledSndDevData peakIdx  
                let rightLiftOff = 
                    match rightLiftOffIdx with 
                    | Option.Some i -> Option.Some (createPeakFeature i xData.[i] yData.[i])
                    | Option.None   -> Option.None
                apex, 
                leftLiftOff,
                rightLiftOff
    
            let getPeaks polOrder ws xData yData whiteNoiseMean peaksContinuous (gaussians: EMGaussian []) = 
                ///
                let negSndDev = savitzky_golay ws polOrder 2 1 yData |> Array.ofSeq |> Array.map ((*) -1.)  
                ///
                let labeledDataTmp = findPeakExtrema 0. 0. (xData |> Array.ofSeq) negSndDev 
                ///
                let peaks =
                    gaussians           //try without recurring peak finding
                    |> Array.sortBy (fun x -> x.Mode)
                let reCalculatedMode =
                    peaks
                    |> Array.map (fun x -> x.Mode)
                    |> Array.map (fun foundPeak -> xData 
                                                   |> Array.mapi (fun i x -> abs(x-foundPeak),i )
                                                   |> Array.sortBy (fun (x,y) -> x)
                                                   |> Array.head
                                                   |> fun (x,i) -> i, {Meta=Extrema.Positive; Data = xData.[i],negSndDev.[i]}
                                  )
                let (apex,leftLiftOff,rightLiftOff) =
                    reCalculatedMode
                    |> Array.map (fun (i,_) -> characterizePeak xData yData labeledDataTmp i)
                    |> fun x -> x
                    |> Array.unzip3
                let peakRanges = peaks |> Array.map (fun emgaussian -> (fst (elongateEMG -1. emgaussian 1000.), fst (elongateEMG 1. emgaussian 1000.) ))
                let xDataForEachPeak = if peaksContinuous = true then labelPeaksContinuous xData apex peakRanges whiteNoiseMean peaks else labelPeaks xData apex peakRanges whiteNoiseMean peaks
                                       |> Array.filter (fun (xPos,xInd,peakID) -> peakID <> -10)
                                       |> fun x -> (*printfn "peakData.Length = %i" x.Length*)
                                                   //printfn "peakNumber from peakData.Length = %i" (Array.distinctBy (fun (xPos,xInd,peakID) -> peakID) x).Length
                                                   if (Array.distinctBy (fun (xPos,xInd,peakID) -> peakID) x).Length <> peaks.Length then printfn "change TestVariables"
                                                                                                                                          testVariable <- x//failwith (sprintf "Error 05; different array lengths: peaksData = %i; peaks = %i" x.Length peaks.Length)
                                                                                                                                          testVariable2 <- peakRanges
                                                                                                                                          testVariable3 <- gaussians
                                                   Array.groupBy (fun (xData,xInd,peakId) -> peakId) x
                                       //|> Array.groupBy (fun (xData,xInd,peakId) -> peakId)
                                       |> Array.sort
                                       |> Array.map (fun (header,data) -> data)
                                       |> JaggedArray.map (fun ((xDataValue:float),xInd,peakId) -> xDataValue, xInd)
                let peaksData =
                    xDataForEachPeak
                    |> JaggedArray.map (fun (xValue,xInd) -> xValue,yData.[xInd] )
                    |> Array.map (Array.unzip)
                
                let parseToPeakFeature (xValue,xInd) =
                    createPeakFeature xInd xValue yData.[xInd]
                if apex.Length <> peaks.Length then failwith "Error 04"
                if xDataForEachPeak.Length <> peaks.Length then failwith (sprintf "Error 03; different array lengths: peaksData = %i; peaks = %i" peaksData.Length peaks.Length)
                [| for i = 0 to (peaks.Length-1) do 
                    yield createIdentifiedPeak  apex.[i]
                                                leftLiftOff.[i] 
                                                (parseToPeakFeature xDataForEachPeak.[i].[0]) 
                                                rightLiftOff.[i] 
                                                (parseToPeakFeature xDataForEachPeak.[i].[(xDataForEachPeak.[i].Length-1)]) 
                                                (fst peaksData.[i]) 
                                                (snd peaksData.[i])|]

    open FSharpAux
    open AuxFunctions
    open AuxFunctions.AuxTypes
             
    ///creating boundaries for array
    let private createFrameArray meanFrom meanTo windowSize =
        let padBytes = 50.*windowSize
        let paddedStart = meanFrom - padBytes
        let paddedEnd = meanTo + padBytes
        let createFrameArray =
            [|floor paddedStart .. windowSize .. ceil paddedEnd|]
        let getFrameArrayLength = createFrameArray.Length
        let createZerosForTupleArray = Array.create getFrameArrayLength 0.
        Array.zip createFrameArray createZerosForTupleArray    

    ///
    let private addWhiteNoise meanWhiteNoise whiteNoiseStdv (gausArr:(float*float)[]) =
        let createWhiteNoiseArray = AuxDistributions.sampleArrOfGaussian gausArr.Length meanWhiteNoise whiteNoiseStdv
        gausArr
        |> fun gausArr -> Array.map2 (fun (position,intensity) whiteNoise -> position, whiteNoise
                                     ) gausArr createWhiteNoiseArray

    module FromRealData =

        open RealDataBinning

        ///Returns "numberOfPeaks"-amount of random peaks.
        ///This function uses real data to try and simulate the measured parameter distributions for artificial peak arrays.
        ///It will calculate the correlation of Gaus/Emg-like peaks and create those peaks according to the measured parameters mean,sigma,intensity,tau.
        ///"windowSize" represents the simulated accuracy and the x-axis difference between two closest points. 
        ///White Noise is created gaussian-distributed around "whiteNoiseMean" with a standard deviation of "whiteNoiseStdv". This will be added to every point.
        ///"minDistancePeaks" is a to the gaussians-sigma-relative minimum distance between any two gaussian distributions in the array.
        ///"minDifferenceIntensity" is a to the apex-point-relative minimum size difference for all apex intensities for all gaussian distributions in the array.
        ///ExmpInput: createPeakArray 10 false NumberOne 0.3 0.01 1000. 4000. 2. 2. 
        ///Filters all given Sigma < 0.05; emg function would error if that would be the case
        let createPeakArray numberOfPeaks peaksContinuous (realDataModel: RealData) intensityNoiseStdv windowSize (whiteNoiseMean:float) whiteNoiseStdv minDistancePeaks minDifferenceIntensity = 
            
            if minDifferenceIntensity <= 1. then failwith "minDifferenceIntensity should not be below/equal 1, this can lead to problems with peakdetermination"
            let blankFrameArray = createFrameArray 0. realDataModel.GausDataInfo.Runtime windowSize
            /////////Start main functions/////////////////////////////////////////////////////////////////////////////////////////////////////////////
            
            /// n = number of wanted peaks; realDataModel = ID (1;); whiteNoiseMean; intensityNoiseStdv; minDistMean; minIntensityDifference
            let createRandomEMG n (realDataModel: RealData) whiteNoiseMean intensityNoiseStdv minDistMean minIntensityDifference =
                    
                //printfn "Start with framework-functions for easier access later."
                let propabilityGaus = realDataModel.GausDataInfo.NumberOfPeaks / (realDataModel.GausDataInfo.NumberOfPeaks + realDataModel.EMGDataInfo.NumberOfPeaks) 
                let target = FSharp.Stats.Random.rndgen
                let emptyMap = Map.empty
            
                //printfn "Start with creating all GausDistributions (loopingCreateOneGaus)"
                let mutable (info: EMGaussian []) = [||]
                let rec loopingCreateOneEMG count usedValuesForGausGeneration (mapOfUsedGaus: Map<float,(float*float*float*float)>) =
            
                    //TODO: Find better name
                    let rec loopingNoOverlapEMGProduction (map: Map<float,(float*float*float*float)>) countOverlap =
            
                        //printfn "%A - Repeats to find non-overlap random data for new gaus." countOverlap
            
                        if countOverlap >= 200
                        then printfn "Not enough peaks without overlap found. Stopped at %i gaussian distributions." count
                             (-20.,-20.,-500.,-500.,-500.)
                        else if target.NextFloat() >= propabilityGaus 
                             then (*printfn "do one EMG"*)
                                  (  (getRndFromMap realDataModel.EMGDataInfo.IntensityDistribution realDataModel.EMGDataInfo.IntensityBandwidth 0),
                                     ((getRndFromMap realDataModel.EMGDataInfo.MeanDistribution realDataModel.EMGDataInfo.MeanBandwidth 10)/60.), 
                                     (getRndFromMap realDataModel.EMGDataInfo.SigmaDistribution realDataModel.EMGDataInfo.SigmaBandwidth 10), 
                                     (getRndFromMap realDataModel.EMGDataInfo.TauDistribution realDataModel.EMGDataInfo.TauBandwidth 10),
                                     0.
                                  )  
                             else (*printfn "do one Gauss"*)
                                  (  (getRndFromMap realDataModel.GausDataInfo.IntensityDistribution realDataModel.GausDataInfo.IntensityBandwidth 0),
                                     ((getRndFromMap realDataModel.GausDataInfo.MeanDistribution realDataModel.GausDataInfo.MeanBandwidth 10)/60.), 
                                     (getRndFromMap realDataModel.GausDataInfo.SigmaDistribution realDataModel.GausDataInfo.SigmaBandwidth 10), 
                                     0.,
                                     0.
                                  )    
                        |> fun (intensityVal, meanVal, sigmaVal, tauVal, zero) -> 
                            if (intensityVal, meanVal, sigmaVal, tauVal, zero) = (-20.,-20.,-500.,-500.,-500.)
                            then (-20.,-20.,-500.,-500.,-500.)
                            else (intensityVal, meanVal, sigmaVal, tauVal,(if tauVal > 0. then (AuxDistributions.modeOfEMG meanVal sigmaVal tauVal) else meanVal))
                                 |> fun (intensityVal, meanVal, sigmaVal, tauVal, modeVal) ->
                                        if (intensityVal, meanVal, sigmaVal, tauVal, modeVal) = (-20.,-20.,-500.,-500.,-500.) 
                                        then (-20.,-20.,-500.,-500.,-500.)
                                        elif (AuxFunctions.AuxDistributions.emg meanVal sigmaVal intensityVal tauVal modeVal) <= whiteNoiseMean 
                                        then loopingNoOverlapEMGProduction mapOfUsedGaus countOverlap
                                        elif tauVal > 2.5 * sigmaVal 
                                        then loopingNoOverlapEMGProduction mapOfUsedGaus countOverlap
                                        elif sigmaVal < 0.05
                                        then loopingNoOverlapEMGProduction mapOfUsedGaus countOverlap
                                        else //Increasing the numbers before sigmaVal increases the distance between two possible peaks made by this function.
                                             if (Map.exists (fun meanMap (sigmaMap,intensityMap,tauMap,modeMap) -> meanMap >= (meanVal-(minDistMean*(abs sigmaVal)))
                                                                                                                   && meanMap <= (meanVal+(minDistMean*(abs sigmaVal)))
                                                            ) map
                                                ) = true 
                                                || (Map.exists (fun meanMap (sigmaMap,intensityMap,tauMap,modeMap) -> meanVal >= (meanMap-(minDistMean*(abs sigmaMap)))
                                                                                                                      && meanVal <= (meanMap+(minDistMean*(abs sigmaMap)))
                                                               ) map 
                                                   ) = true
                                             then loopingNoOverlapEMGProduction mapOfUsedGaus (countOverlap+1)
                                             else if (Map.exists (fun meanMap (sigmaMap,intensityMap,tauMap,modeMap) -> (AuxFunctions.AuxDistributions.emg meanVal sigmaVal intensityVal tauVal modeVal) <= (minIntensityDifference*(AuxFunctions.AuxDistributions.emg meanMap sigmaMap intensityMap tauMap modeVal))
                                                                 ) map
                                                     ) = true
                                                     || (Map.exists (fun meanMap (sigmaMap,intensityMap,tauMap,modeMap) -> (AuxFunctions.AuxDistributions.emg meanMap sigmaMap intensityMap tauMap modeMap) <= (minIntensityDifference*(AuxFunctions.AuxDistributions.emg meanVal sigmaVal intensityVal tauVal modeMap))
                                                                    ) map
                                                        ) = true
                                                  then loopingNoOverlapEMGProduction mapOfUsedGaus (countOverlap+1)
                                                  else (intensityVal,meanVal,sigmaVal,tauVal,modeVal)
                                            
                    //printfn "Start with gausDistribution %A." count
            
                    if count < n
                    then loopingNoOverlapEMGProduction mapOfUsedGaus 0
                        ///(-20.,-20.,-500.,-500.,-500.) is given if no non-overlap gaus-distributions are found withing a 100 tries. In that Case the function should
                        ///just give the result
                        |> fun x -> if x <> (-20.,-20.,-500.,-500.,-500.) 
                                    then x |> fun gausInfo -> loopingCreateOneEMG (count+1) (Array.append [|gausInfo|] usedValuesForGausGeneration) (gausInfo |> (fun (int,mean,sigma,tau,mode) -> Map.add mean (sigma,int,tau,mode) mapOfUsedGaus))
                                    elif x = (-20.,-20.,-500.,-500.,-500.) 
                                    then info <- (usedValuesForGausGeneration |> Array.map (fun (int,mean,sigma,tau,mode) -> createEMGaussian mean sigma int tau mode
                                                                                          )
                                                 )
                                    else failwith "unknown gausinformation pattern"
                    else info <- (usedValuesForGausGeneration |> Array.map (fun (int,mean,sigma,tau,mode) -> createEMGaussian mean sigma int tau mode
                                                                          )
                                 )
            
                let createEMGaussArr (gausArray: (float*float)[]) =
                
                    let calcDistributionArray =
                        info 
                        |> Array.map (fun emgaussian  -> gausArray 
                                                         |> Array.map (fun (x,y) -> let yAtX = AuxFunctions.AuxDistributions.emg emgaussian.Mean emgaussian.Sigma emgaussian.Intensity emgaussian.Tau x
                                                                                    let generateRandomIntensityNoise = AuxFunctions.AuxDistributions.sampleOfGaussian 0. intensityNoiseStdv
                                                                                    x, yAtX, yAtX + (yAtX*generateRandomIntensityNoise)
                                                                      )
                                     )
                        |> Array.concat
                        |> Array.groupBy (fun (xPos,yInt,yNoise) -> xPos)
                        |> Array.map (fun (header,data) -> (Array.unzip3 data) |> fun (xPos,yInt,yNoise) -> xPos.[0], Array.sum yInt, Array.sum yNoise  
                                     )
                        |> Array.sort
                        |> Array.unzip3
                        |> fun (xPos, yInt, yNoise) -> Array.zip xPos yInt, Array.zip xPos yNoise
                        |> fun (gausArr,noiseArr) -> createSimulatedTestXic gausArr info noiseArr
                    calcDistributionArray
                
                loopingCreateOneEMG 0 [||] emptyMap
                |> fun _ -> createEMGaussArr blankFrameArray
        
            ///
            let simXics = createRandomEMG numberOfPeaks realDataModel whiteNoiseMean intensityNoiseStdv minDistancePeaks minDifferenceIntensity

            simXics
            //|> fun gausArr -> verifyPeaks gausArr windowSize (findPeaks gausArr)
            //|> fun verifiedGaussians -> if verifiedGaussians.Length = numberOfPeaks 
            //                                        then simXics.DistributionArray, simXics.NoiseArray, verifiedGaussians
            //                                        else recalculateGaus whiteNoiseMean intensityNoiseStdv blankFrameArray verifiedGaussians
            |> fun x -> x.DistributionArray, x.NoiseArray, x.EMGaussians
            |> fun (gausArr,gausNoise,gaussians) -> createSimulatedXic  (addWhiteNoise whiteNoiseMean whiteNoiseStdv gausArr)
                                                                        gausArr 
                                                                        gausNoise 
                                                                        gaussians 
                                                                        (gausArr
                                                                        |> Array.unzip
                                                                        |> fun (xData,yData) -> AuxPeakFinding.getPeaks 3 29 xData yData whiteNoiseMean peaksContinuous gaussians
                                                                        )
    module FromRandomGenerator = 

        open AuxFunctions.AuxDistributions
        ///Returns "numberOfPeaks"-amount of random gaus/emg distributions.
        ///Distributions are built according to the input for "emgPropability meanFrom meanTo intensityFrom intensityTo intensityNoiseStdv sigmaMean sigmaStdv tauMean tauStdv".
        ///Propability (from 0. to 1.) for a emg distribution (otherwise gauss) is given as "emgPropability".
        ///Means of these distributions will lie between "meanFrom" and "meanTo" on the x-axis,
        ///with gaussian distributed sigmas(and taus for emg) around "sigmaMean"(/tauMean) with "sigmaStdv"(/tauStdv) as standard deviation.
        ///Intensities are randomly generated between intensityFrom and intensityTo, similiar to a exponential decreasing function, but with an additional low starting propability.
        ///"windowSize" represents the simulated accuracy and the x-axis difference between two closest points. 
        ///White Noise is created gaussian distributed around "whiteNoiseMean" with a standard deviation of "whiteNoiseStdv". This will be added to every point.
        ///"minDistancePeaks" is a to the gaussians-sigma-relative minimum distance between any two gaussian distributions in the array.
        ///"minDifferenceIntensity" is a to the apex-point-relative minimum size difference for all apex intensities for all gaussian distributions in the array.
        ///"rndIntensityGenerator" can be used to give more realistic-randomly-distributed intensity values; "rndIntensityGenerator" should be a generator for n random intensity values (Exmp: getXicDistributedBins)
        ///ExmpInput: createRndPeakArray 10 false 0.55 0. 50. 10000. 999999. 0.3 0.3 0.15 0.5 0.2 0.01 1000. 4000. 1. 1. 
        ///Filters all given Sigma < 0.05; emg function would error if that would be the case
        let createRndPeakArray numberOfPeaks peaksContinuous (emgPropability: float) meanFrom meanTo intensityFrom intensityTo intensityNoiseStdv sigmaMean sigmaStdv tauMean tauStdv windowSize (whiteNoiseMean:float) whiteNoiseStdv minDistancePeaks minDifferenceIntensity = 
            
            if minDifferenceIntensity <= 1. then failwith "minDifferenceIntensity should not be below/equal 1, this can lead to problems with peakdetermination"
            let intensityDistribution = RealData.getStandardIntensityDistribution (RealData.getRelativeFilePath "material\StandardIntensityDistribution.txt")
            let blankFrameArray = createFrameArray meanFrom meanTo windowSize

            /////////Start main functions/////////////////////////////////////////////////////////////////////////////////////////////////////////////
            
            /// n = wanted number of peaks; emgPropability; whiteNoiseMean; intensityNoiseStdv; minDistMean; minIntensityDifference
            let createRandomEMG n emgPropability whiteNoiseMean intensityNoiseStdv minDistMean minIntensityDifference =
                    
                
                //printfn "Start with framework-functions for easier access later."
                let target = FSharp.Stats.Random.rndgen
                let emptyMap = Map.empty
            
                //printfn "Start with creating all GausDistributions (loopingCreateOneGaus)"
                let mutable (info: EMGaussian[]) = [||]
                let rec loopingCreateOneEMG count (usedValuesForGausGeneration:(float*float*float*float*float)[]) (mapOfUsedGaus: Map<float,(float*float*float*float)>) =
            
                    //TODO: Find better name
                    let rec loopingNoOverlapEMGProduction (map: Map<float,(float*float*float*float)>) countOverlap =
            
                        //printfn "%A - Repeats to find non-overlap random data for new gaus." countOverlap
            
                        if countOverlap >= 200 
                        then printfn "Not enough peaks without overlap found. Stopped at %i gaussian distributions." count
                             (-20.,-20.,-500.,-500.,-500.)
                        else if target.NextFloat() <= emgPropability 
                             then(* printfn "do one EMG"*)
                                  (  (genRandomIntensity intensityDistribution intensityFrom intensityTo),
                                     (genRandomMean meanFrom meanTo), 
                                     (sampleOfGaussian sigmaMean sigmaStdv), 
                                     (sampleOfGaussian tauMean tauStdv),
                                     0.
                                  )  
                             else (*printfn "do one Gaus"*)
                                  (  (genRandomIntensity intensityDistribution intensityFrom intensityTo),
                                     (genRandomMean meanFrom meanTo), 
                                     (sampleOfGaussian sigmaMean sigmaStdv), 
                                     0.,
                                     0.
                                  )
                        |> fun (intensityVal, meanVal, sigmaVal, tauVal, zero) -> 
                            if (intensityVal, meanVal, sigmaVal, tauVal, zero) = (-20.,-20.,-500.,-500.,-500.)
                            then (-20.,-20.,-500.,-500.,-500.)
                            else (intensityVal, meanVal, sigmaVal, tauVal,(if tauVal > 0. then (AuxDistributions.modeOfEMG meanVal sigmaVal tauVal) else meanVal))
                                 |> fun (intensityVal, meanVal, sigmaVal, tauVal, modeVal) ->
                                        if (intensityVal, meanVal, sigmaVal, tauVal, modeVal) = (-20.,-20.,-500.,-500.,-500.) 
                                        then (-20.,-20.,-500.,-500.,-500.)
                                        elif (AuxFunctions.AuxDistributions.emg meanVal sigmaVal intensityVal tauVal modeVal) <= whiteNoiseMean 
                                        then loopingNoOverlapEMGProduction mapOfUsedGaus countOverlap
                                        elif tauVal > 2.5 * sigmaVal 
                                        then loopingNoOverlapEMGProduction mapOfUsedGaus countOverlap
                                        elif sigmaVal < 0.05
                                        then loopingNoOverlapEMGProduction mapOfUsedGaus countOverlap
                                        else //Increasing the numbers before sigmaVal increases the distance between two possible peaks made by this function.
                                             if (Map.exists (fun meanMap (sigmaMap,intensityMap,tauMap,modeMap) -> meanMap >= (meanVal-(minDistMean*(abs sigmaVal)))
                                                                                                                   && meanMap <= (meanVal+(minDistMean*(abs sigmaVal)))
                                                            ) map
                                                ) = true 
                                                || (Map.exists (fun meanMap (sigmaMap,intensityMap,tauMap,modeMap) -> meanVal >= (meanMap-(minDistMean*(abs sigmaMap)))
                                                                                                                      && meanVal <= (meanMap+(minDistMean*(abs sigmaMap)))
                                                               ) map 
                                                   ) = true
                                             then loopingNoOverlapEMGProduction mapOfUsedGaus (countOverlap+1)
                                             else if (Map.exists (fun meanMap (sigmaMap,intensityMap,tauMap,modeMap) -> (AuxFunctions.AuxDistributions.emg meanVal sigmaVal intensityVal tauVal modeVal) < (minIntensityDifference*(AuxFunctions.AuxDistributions.emg meanMap sigmaMap intensityMap tauMap modeVal))
                                                                 ) map
                                                     ) = true
                                                     || (Map.exists (fun meanMap (sigmaMap,intensityMap,tauMap,modeMap) -> (AuxFunctions.AuxDistributions.emg meanMap sigmaMap intensityMap tauMap modeMap) < (minIntensityDifference*(AuxFunctions.AuxDistributions.emg meanVal sigmaVal intensityVal tauVal modeMap))
                                                                    ) map
                                                        ) = true
                                                  then loopingNoOverlapEMGProduction mapOfUsedGaus (countOverlap+1)
                                                  else (intensityVal,meanVal,sigmaVal,tauVal,modeVal)
                                            
                    //printfn "Start with gausDistribution %A." count
            
                    if count < n
                    then loopingNoOverlapEMGProduction mapOfUsedGaus 0
                        ///(-20.,-20.,-500.,-500.,-500.) is given if no non-overlap gaus-distributions are found withing a 100 tries. In that Case the function should
                        ///just give the result
                        |> fun x -> if x <> (-20.,-20.,-500.,-500.,-500.) 
                                    then x |> fun gausInfo -> loopingCreateOneEMG (count+1) (Array.append [|gausInfo|] usedValuesForGausGeneration) (gausInfo |> (fun (int,mean,sigma,tau,mode) -> Map.add mean (sigma,int,tau,mode) mapOfUsedGaus))
                                    elif x = (-20.,-20.,-500.,-500.,-500.) 
                                    then info <- (usedValuesForGausGeneration |> Array.map (fun (int,mean,sigma,tau,mode) -> createEMGaussian mean sigma int tau mode
                                                                                          )
                                                 )
                                    else failwith "unknown gausinformation pattern"
                    else info <- (usedValuesForGausGeneration |> Array.map (fun (int,mean,sigma,tau,mode) -> createEMGaussian mean sigma int tau mode
                                                                          )
                                 )
            
                let createEMGaussArr (*count *)(gausArray: (float*float)[]) (*gausNoiseArrayList*) =
                
                    let calcDistributionArray =
                        info 
                        |> Array.map (fun emgaussian  -> gausArray 
                                                         |> Array.map (fun (x,y) -> let yAtX = AuxFunctions.AuxDistributions.emg emgaussian.Mean emgaussian.Sigma emgaussian.Intensity emgaussian.Tau x
                                                                                    let generateRandomIntensityNoise = sampleOfGaussian 0. intensityNoiseStdv
                                                                                    x, yAtX, yAtX + (yAtX*generateRandomIntensityNoise)
                                                                      )
                                     )
                        |> Array.concat
                        |> Array.groupBy (fun (xPos,yInt,yNoise) -> xPos)
                        |> Array.map (fun (header,data) -> (Array.unzip3 data) |> fun (xPos,yInt,yNoise) -> xPos.[0], Array.sum yInt, Array.sum yNoise  
                                     )
                        |> Array.sort
                        |> Array.unzip3
                        |> fun (xPos, yInt, yNoise) -> Array.zip xPos yInt, Array.zip xPos yNoise
                        |> fun (gausArr,noiseArr) -> createSimulatedTestXic gausArr info noiseArr
                    calcDistributionArray
                
                loopingCreateOneEMG 0 [||] emptyMap
                |> fun _ -> createEMGaussArr blankFrameArray
           
            ///
            let simXics = createRandomEMG numberOfPeaks emgPropability whiteNoiseMean intensityNoiseStdv minDistancePeaks minDifferenceIntensity

            simXics
            //|> fun gausArr -> verifyPeaks gausArr windowSize (findPeaks gausArr)
            //|> fun verifiedGaussians -> if verifiedGaussians.Length = numberOfPeaks 
            //                                        then simXics.DistributionArray, simXics.NoiseArray, verifiedGaussians
            //                                        else recalculateGaus whiteNoiseMean intensityNoiseStdv blankFrameArray verifiedGaussians
            |> fun x -> x.DistributionArray, x.NoiseArray, x.EMGaussians
            |> fun (gausArr,gausNoise,gaussians) -> createSimulatedXic  (addWhiteNoise whiteNoiseMean whiteNoiseStdv gausArr)
                                                                        gausArr 
                                                                        gausNoise 
                                                                        gaussians 
                                                                        (gausArr
                                                                        |> Array.unzip
                                                                        |> fun (xData,yData) -> AuxPeakFinding.getPeaks 3 29 xData yData whiteNoiseMean peaksContinuous gaussians
                                                                        )     

