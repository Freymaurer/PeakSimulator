namespace PeakSimulator


open FSharp.Stats
open FSharp.Stats.Distributions
open FSharpAux

////////////////////////////////Get Real Intensities////////////////////////////////////////////////////////

module RealDataBinning =

    ///Returns 'randomly' a bin according to real distribution propabilites. 
    ///This function bins Xic-intentisities and calculates the propabily for each bin.
    ///The output are n, 'randomly' (according to their propability) chosen, bins, whereas the output value is the exact middle-point of the bin; x = 10 equals a bucket from 1 to 20.
    ///"n" = the amount of 'randomly' chosen bins.
    ///"Values" as a float array with the data that should be binned. 
    let getRealDistributedBins (values : float []) =
    
        let doIntelligentBinning (intensities:float []) = 
            intensities 
            |> fun intensity -> intensity,(Bandwidth.scottNormal intensity)
            |> fun (intensity,bandwidth) ->  Distributions.Frequency.create bandwidth intensity, bandwidth
       
        values
        |> doIntelligentBinning //might choose the bucketsize too big
        /////here starts testing; use this to get normalized bin propabilities
        //|> fun (x,y)-> x
        //|> Map.toArray
        //|> fun x -> x
        //|> Array.unzip
        //|> fun (size,amount) -> size, Array.map (fun x -> float x / (float (Array.sum amount)
        //                                                            )
        //                                        ) amount
        /////here ends testing
    
    let getRndFromMap (map:Map<float,int>) (bandwidth:float) digitsToRound(*n*) =
        
        ///random generator
        let target = FSharp.Stats.Random.rndgen
        let doNTimes n = 
            [|0..n|]
            |> Array.map (fun x -> target.NextFloat())
            |> Array.head
    
        ///needs random generator, picks value from pmf according to its probability
        let getRandomValueFromDistribution (pmf:Map<float,float>) rndValue = 
            if pmf.Count <= 0 then raise (System.Exception("Pmf contains no values") )  
            let x,y =
                pmf
                |> Seq.scan (fun state kv -> (kv.Key, kv.Value + snd state)) (0.,0.)
                |> fun x -> x
                |> Seq.find (fun (x,y) -> y >= rndValue)
            x
    
        ///
        let getBinSize (bandwidth:float) (midpoint: float) =
            midpoint
            |> fun x -> x, bandwidth/2.
            |> fun (midpoint,smallesBucket) -> midpoint |> (fun x -> x-smallesBucket, x+smallesBucket) 
    
        ///
        let genRandomIntensityFromBuckets (bucket: (float*float)) =
            bucket
            |> fun (min,max) -> (target.NextFloat() * (max-min)) + min
            |> Math.round digitsToRound
    
        map
        |> Empirical.ofHistogram
        |> fun pmf -> ( (doNTimes 0) |> (fun rndVal -> getRandomValueFromDistribution pmf rndVal
                                        )
                      )
        |> getBinSize bandwidth
        |> genRandomIntensityFromBuckets
    