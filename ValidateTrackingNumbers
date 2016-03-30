//Given a string of a suspected UPS tracking number, the string will be validated with the UPS checksum
let upsValidate (s:string) =
   let upsUpperString = s.ToUpper()
        let upsCharArray = upsUpperString.ToCharArray()
        let upsCharConvert =
            Array.sub upsCharArray 2 16
            |> Array.map
                (fun x->
                    match x with
                    | '0' | 'I' | 'S'->  0
                    | '1' | 'J' | 'T'-> 1
                    | '2' | 'A' | 'K' | 'U'-> 2
                    | '3' | 'B' | 'L' | 'V'-> 3
                    | '4' | 'C' | 'M' | 'W'-> 4
                    | '5' | 'D' | 'N' | 'X'-> 5
                    | '6' | 'E' | 'O' | 'Y'-> 6
                    | '7' | 'F' | 'P' | 'Z'-> 7
                    | '8' | 'G' | 'Q'-> 8
                    | '9' | 'H' | 'R'-> 9
                )
        let upsOddSum =
            upsCharConvert
            |> Array.mapi(fun i x-> if i % 2 <> 0 then Some(x) else None ) 
            |> Array.choose id //at every even index we gave a some option type, this creates a new array of just the Somes
            |> Array.sum
        let upsEvenSum =
            upsCharConvert
            |> Array.mapi(fun i x-> if i % 2 = 0 then Some(x) else None ) 
            |> Array.choose id //at every even index we gave a some option type, this creates a new array of just the Somes
            |> Array.sum
        let upsCheckSum = if (upsOddSum + (upsEvenSum * 2)) % 10 = 0 then true else false
        upsCheckSum



    //Given a string of a suspected FEDEX tracking number, the string will be validated with the UPS checksum
    let fedexValidate(s: string) =
        let fedexCharConvert =
            s.ToCharArray()
            |> Array.map
                (fun x->
                    match x with
                    | '0'-> 0
                    | '1'-> 1
                    | '2'-> 2
                    | '3'-> 3
                    | '4'-> 4
                    | '5'-> 5
                    | '6'-> 6
                    | '7'-> 7
                    | '8'-> 8
                    | '9'-> 9
                )
        let fedex3 =
            fedexCharConvert
            |> Array.mapi(fun i x-> if i % 3 = 0 then Some(x) else None)
            |> Array.choose id
            |> Array.map(fun x->x * 3)
            |> Array.sum
        let fedex1 =
            fedexCharConvert
            |> Array.mapi(fun i x-> if i % 3 = 1 then Some(x) else None)
            |> Array.choose id
            |> Array.map(fun x->x * 1)
            |> Array.sum
        let fedex7 =
            fedexCharConvert
            |> Array.mapi(fun i x-> if (i % 3 = 2 && i <> 11) then Some(x) else None)
            |> Array.choose id
            |> Array.map(fun x->x * 7)
            |> Array.sum
        let fedexCheckSum = if ((fedex3 + fedex1 + fedex7) % 11)% 10 = fedexCharConvert.[11] then true else false
        fedexCheckSum
