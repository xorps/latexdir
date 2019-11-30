module Search

open System.Net.Http
open System.Threading.Tasks
open FSharp.Control.Tasks.V2.ContextInsensitive
open Newtonsoft.Json
open Digits
open Number.Values

type HttpStatusCode = System.Net.HttpStatusCode

type ServiceError =
    | HttpError of exn
    | UnhandledStatusCode of HttpStatusCode

type JSONError =
    | NoMatchingPackage
    | ParseError of exn

type SearchError =
    | InvalidInput
    | NoResults
    | JSONError of JSONError
    | ServiceError of ServiceError
    | EmptyGetString of SearchError []
    | EmptyJson of SearchError []
    | DBError of DB.DBError
    | DBErrors of DB.DBError []
    | FailedGetLatexInfo of SearchError []

type SearchResult<'T> = Result<'T, SearchError>
type SearchTask<'T> = Task<SearchResult<'T>>

let ok_or err = function
    | Some a -> Ok a
    | None -> Error err

let query (ndc : Barcode.NDC_10): SearchTask<Barcode.NDC_10 * string> = task {
    let url ndc = sprintf "https://api.fda.gov/drug/ndc.json?search=packaging.package_ndc:\"%s\"&limit=1" (Barcode.NDC_10.AsString ndc)
    use client = new HttpClient()
    try
        let! res = client.GetAsync (url ndc)
        match res.StatusCode with
        | HttpStatusCode.OK ->
            let! body = res.Content.ReadAsStringAsync()
            return Ok (ndc, body)
        | HttpStatusCode.NotFound -> return Error NoResults
        | code -> return Error (ServiceError (UnhandledStatusCode code))
    with e -> return Error (ServiceError (HttpError e))
}

module DTO =
    type Results = {skip: int; limit: int; total: int}
    type Meta = {disclaimer: string; terms: string; license: string; last_updated: string; results: Results}
    type Packaging = {marketing_start_date: string; package_ndc: string; description: string; sample: bool}
    type Result = {generic_name: string; labeler_name: string; brand_name: string; packaging: Packaging []}
    type T = {meta: Meta; results: Result []}

let readJSON (ndc : Barcode.NDC_10, json : string): SearchResult<View.Product> = 
    try
        let dto = JsonConvert.DeserializeObject<DTO.T> json
        let res = dto.results.[0]
        match res.packaging |> Array.tryFind (fun it -> it.package_ndc = Barcode.NDC_10.AsString ndc) with
        | Some pkg -> Ok {BrandName = res.brand_name; NDC = pkg.package_ndc; GenericName = res.generic_name; Manufacturer = res.labeler_name}
        | None -> Error (JSONError NoMatchingPackage)
    with e -> Error (JSONError (ParseError e))

let permutations (query : string): SearchResult<Barcode.NDC_10 []> = 
    digits query _10 
    |> Option.bind Barcode.possible
    |> Option.orElseWith (fun () -> Barcode.Regex.tryParse query |> Option.map (Array.create 1))
    |> ok_or InvalidInput

let join_results<'T, 'E, 'B> (err : 'E [] -> 'B) (r : Result<'T, 'E> []): Result<'T [], 'B> =
    let successes = r |> Array.choose (fun it -> match it with | Ok a -> Some a; | _ -> None)
    let errors = r |> Array.choose (fun it -> match it with | Error e -> Some e; | _ -> None)
    if Array.isEmpty successes then
        Error (err errors)
    else
        Ok successes

(*
module APIv1 =
    open Control
    open Control.TaskResultBuilder

    let private error_view = function
    | InvalidInput -> View.showText "Invalid Input"
    | NoResults -> View.showText "No Results"
    | EmptyGetString e -> View.showText ("No Results - Empty Get String: " + (e |> Array.map Control.Object.ToString |> String.concat ","))
    | _ as e -> View.showText ("Error - " + (e.ToString()))
    
    let private view = function
    | Ok products -> View.products (products)
    | Error err -> error_view err

    let api (searchTerms : string) = task {
        let! res = taskresult {
            let! ndcs = Task.FromResult (permutations searchTerms)
            let! ndcs = Task.WhenAll (ndcs |> Array.map query) |> Task.map (join_results EmptyGetString)
            let! ndcs = Task.FromResult (ndcs |> Array.map readJSON |> join_results EmptyJson)
            return ndcs
        }
        return View.render (view res)
    }
*)

type View.LatexInfo with
    static member From: (DB.Row option -> View.LatexInfo) = function
        | Some r -> if r.latex_free then View.LatexFree else View.ContainsLatex
        | None -> View.NoData

module APIv2_Helpers = 
    open Control

    let getFDAInfo barcodes =
        barcodes 
        |> Array.map query
        |> Task.WhenAll
        |> Task.map (join_results EmptyGetString)
        |> Task.map (Result.map (Array.map readJSON))
        |> Task.map (Result.bind (join_results EmptyJson))

    let getLatexInfo conn barcodes =
        barcodes
        |> Array.map Barcode.NDC_10.AsString
        |> Array.map (DB.findOne conn)
        |> Task.WhenAll
        |> Task.map (join_results DBErrors)
        |> Task.map (Result.map (Array.map View.LatexInfo.From))

module APIv2 =
    open Control
    open Control.Function
    open Control.TaskResult.Operators
    open APIv2_Helpers

    let run conn s =
        permutations s |> Task.FromResult
        >>= (fun barcodes -> Task.zip (getLatexInfo conn barcodes) (getFDAInfo barcodes) |> Task.map Result.zip)
        <&> (uncurry2 Array.allPairs)

module APIv3 =
    open Control
    open Control.TaskResult.Operators
    
    type Log = string -> obj -> unit
    type Connection = string
    type Query = string

    let getFDAInfo barcodes =
        barcodes 
        |> Array.map query
        |> Task.WhenAll
        |> Task.map (join_results EmptyGetString)
        <&> Array.map readJSON
        |> Task.map (Result.bind (join_results EmptyJson))

    let getLatexInfoArray conn ndc =
        ndc
        |> Array.map (DB.findOne conn)
        |> Task.WhenAll
        |> Task.map (join_results DBErrors)
        <&> Array.map View.LatexInfo.From

    let getLatexInfo conn (p : View.Product) =
        p.NDC
        |> DB.findOne conn
        <&> View.LatexInfo.From
        |> Task.map (Result.mapError DBError)

    let withLatexInfo conn (p : View.Product) =
        getLatexInfo conn p
        <&> fun it -> (p, it)

    let private error_view err log =
        match err with
        | InvalidInput -> View.showText "Invalid Input"
        | NoResults -> View.showText "No Results"
        | _ as e -> 
            do log "search api failed" e
            View.showText "Internal Error - Something went wrong :("
    
    let private view res log =
        match res with
        | Ok products -> View.products (products)
        | Error err -> error_view err log

    let api (conn : Connection) (log : Log) (s : Query) =
        permutations s
        |> Task.FromResult
        >>= getFDAInfo
        <&> Array.map (withLatexInfo conn)
        <&> Task.WhenAll
        >>= Task.map (join_results FailedGetLatexInfo)
        |> Task.map (fun a -> View.render (view a log))
